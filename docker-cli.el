(require 'comint)

(defvar docker-cli-cmd "docker"
  "Docker command")

(defvar docker-cli-exec-arguments '("exec" "-it")
  "Commandline arguments to pass to docker")

(defvar docker-cli-curr-command nil
  "Current selected command")

(defvar docker-cli-commands-alist
  '((psql
     :command "psql"
     :arguments-compose-func docker-cli-psql-arguments)

    (redis-cli
     :command "redis-cli"
     :arguments-compose-func nil)
    )
  "An alist of defined commands that can be ran in docker container.

Each element in the list must be of the following format:

  (COMMAND-KEY FEATURE VALUE)

where COMMAND-KEY is unique value that determins command and can
be displayed in selection when `docker-run' is executed. Each key
is followed by FEATURE-VALUE pairs. Feature can be any of following:

  :command                    Command that will be executed in the
                              Docker container.

  :arguments-compose-func     Function without arguments that will be
                              called in order to fetch all command
                              arguments.

New commands can be supported by adding new element to this list.")

(defvar docker-cli-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-docker'")

(defcustom docker-cli-db-username ""
  "Database username."
  :type 'string
  :group 'DockerCLI)

(defcustom docker-cli-db-name ""
  "Database name"
  :type 'string
  :group 'DockerCLI)

(defcustom docker-cli-host ""
  "Host name"
  :type 'string
  :group 'DockerCLI)

;; This value is for psql. It should be nil here and
;; set depending of command started
(defvar docker-cli-prompt-regexp "^[[:alnum:]_]*=[#>] "
  "Prompt for `run-docker'.")

;; This value is for psql. It should be nil here and
;; set depending of command started
(defvar docker-cli-prompt-cont-regexp "^[[:alnum:]_]*=[#>] "
  "Prompt pattern for continuation prompt.")

(defun docker-cli-psql-arguments ()
  "Composes arguments for running PSQL in docker container"
  (setq docker-cli-db-username (read-string "Username: " docker-cli-db-username))
  (setq docker-cli-db-name (read-string "Database: " docker-cli-db-name))
  (setq docker-cli-host (read-string "Host: " docker-cli-host))
  `("-U" ,docker-cli-db-username "-h" ,docker-cli-host "-P" "pager=off" ,docker-cli-db-name)
  )

(defun docker-cli-select-option (prompt options)
  (ido-completing-read prompt options))

(defun docker-cli-compose-params ()
  (let* ((container (docker-cli-select-option
                     "Container: "
                     (split-string (shell-command-to-string "docker ps --format '{{.Names}}'"))))
         (curr-command-name (docker-cli-select-option
                             "Command: "
                             (mapcar 'symbol-name (mapcar 'car docker-cli-commands-alist))))
         (curr-command (cdr (assoc (intern curr-command-name) docker-cli-commands-alist)))
         (params (apply (plist-get curr-command :arguments-compose-func) nil)))
    (setq params (cons (plist-get curr-command :command) params))
    (setq params (cons container params))
    (setq params (append docker-cli-exec-arguments params))))

(defun run-docker ()
  "Run an inferior instance of `docker' inside Emacs."
  (interactive)
  (let* ((buffer (comint-check-proc "Docker")))

    ;; pop to the "*Docker*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'docker-cli-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Docker*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Docker" buffer
             docker-cli-cmd nil (docker-cli-compose-params))
      (docker-cli-mode))))

(defun docker-cli--initialize ()
  "Helper function to initialize Docker"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (set (make-local-variable 'font-lock-defaults) '(docker-cli-font-lock-keywords t)))

(defconst docker-cli-keywords
  '("select" "from" "where" "into" "order" "group by"))

(defvar docker-cli-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt docker-cli-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `docker-cli-mode'.")

(define-derived-mode docker-cli-mode comint-mode "Docker"
  "Major mode for `run-docker'.

\\<docker-cli-mode-map>"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp
        (if docker-cli-prompt-cont-regexp
            (concat "\\(" docker-cli-prompt-regexp
                    "\\|" docker-cli-prompt-cont-regexp "\\)")
          docker-cli-prompt-regexp))
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(docker-cli-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) docker-cli-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'docker-cli-mode-hook 'docker-cli--initialize)
