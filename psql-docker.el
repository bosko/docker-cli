(require 'sql)
;; https://gist.github.com/Yuhta/f41347064170365fa170

;; Sample of command to execute
;; docker exec -it pg11 psql -U postgres -d sealv2_development
;; sql-postgres params
;; ("-U" "postgres" "-h" "localhost" "-P" "pager=off" "sealv2_development")
(defcustom sql-docker-psql-program "docker"
  "Executable to start PSQL in Docker.
Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-docker-psql-program-options '("exec" "-it")
  "Docker arguments to execut application in the container."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-docker-psql-container ""
  "Name of Docker container to start psql in."
  :type 'string
  :group 'SQL)

(defcustom sql-docker-psql-options '("-P" "pager=off")
  "List of additional options to start psql in Docker container."
  :type '(repeat string)
  :group 'SQL)

(defun sql-comint-docker-psql (product options &optional buf-name)
  "Connect to Docker psql in a comint buffer."
  (let ((params options))
    (sql-get-login-ext 'sql-docker-psql-container "Container name: " nil nil)
    (if (not (string= "" sql-database))
        (setq params (append (list "-d" sql-database) params)))
    (if (not (string= "" sql-password))
        (setq params (append (list "-W" sql-password) params)))
    (if (not (string= "" sql-user))
        (setq params (append (list "-U" sql-user) params)))
    (setq params (cons "psql" params))
    (sql-comint product (append sql-docker-psql-program-options (cons sql-docker-psql-container params)))))

(defun sql-docker-psql (&optional buffer)
  "Run psql in Docker container as an inferior process."
  (interactive "P")
  (sql-product-interactive 'docker-psql buffer))

(let ((product 'docker-psql)
      (display "docker-psql")
      (plist '(:free-software t
               :sqli-program sql-docker-psql-program
               :prompt-regexp "^[[:alnum:]_]*=[#>] "
               :prompt-length 5
               :prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] "
               :sqli-login sql-postgres-login-params
               :sqli-options sql-docker-psql-options
               :sqli-comint-func sql-comint-docker-psql)))
  ;; `sql-add-product' is buggy
  ;; (apply #'sql-add-product product display plist)
  (if (assoc product sql-product-alist)
      (message "Product `%s' is already defined" product)
    (add-to-list 'sql-product-alist
                 (append (list product :name display) plist))))

(provide 'docker-psql)
