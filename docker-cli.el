(require 'comint)

(defvar docker-cmd "docker"
  "Docker command")

(defvar docker-arguments '("exec" "--user" "postgres" "-it" "pg11" "psql")
  "Commandline arguments to pass to docker")

(defvar docker-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-docker'")

;; This value is for psql. It should be nil here and
;; set depending of command started
(defvar docker-prompt-regexp "^[[:alnum:]_]*=[#>] "
  "Prompt for `run-docker'.")

;; This value is for psql. It should be nil here and
;; set depending of command started
(defvar docker-prompt-cont-regexp "^[[:alnum:]_]*=[#>] "
  "Prompt pattern for continuation prompt.")

(defun run-docker ()
  "Run an inferior instance of `docker' inside Emacs."
  (interactive)
  (let* ((docker-program docker-cmd)
         (buffer (comint-check-proc "Docker")))

    ;; pop to the "*Docker*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'docker-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Docker*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Docker" buffer
             docker-program nil docker-arguments)
      (docker-mode))))

(defun docker--initialize ()
  "Helper function to initialize Docker"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (set (make-local-variable 'font-lock-defaults) '(docker-font-lock-keywords t)))

(defconst docker-keywords
  '("select" "from" "where" "into" "order" "group by"))

(defvar docker-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt docker-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `docker-mode'.")

(define-derived-mode docker-mode comint-mode "Docker"
  "Major mode for `run-docker'.

\\<docker-mode-map>"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp
        (if docker-prompt-cont-regexp
            (concat "\\(" docker-prompt-regexp
                    "\\|" docker-prompt-cont-regexp "\\)")
          docker-prompt-regexp))
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(docker-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) docker-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'docker-mode-hook 'docker--initialize)
