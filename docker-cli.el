(require 'comint)

(defvar docker-cli-cmd "docker"
  "Docker command")

(defvar docker-cli-arguments '("exec" "--user" "postgres" "-it" "pg11" "psql" "-P" "pager=off")
  "Commandline arguments to pass to docker")

(defvar docker-cli-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-docker'")

;; This value is for psql. It should be nil here and
;; set depending of command started
(defvar docker-cli-prompt-regexp "^[[:alnum:]_]*=[#>] "
  "Prompt for `run-docker'.")

;; This value is for psql. It should be nil here and
;; set depending of command started
(defvar docker-cli-prompt-cont-regexp "^[[:alnum:]_]*=[#>] "
  "Prompt pattern for continuation prompt.")

(defun run-docker ()
  "Run an inferior instance of `docker' inside Emacs."
  (interactive)
  (let* ((docker-cli-program docker-cli-cmd)
         (buffer (comint-check-proc "Docker")))

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
             docker-cli-program nil docker-cli-arguments)
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
