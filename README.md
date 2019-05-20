## docker-runner-cli ##

Running various CLI in Docker within Emacs buffer

## Installation ##

The best way to install it is from MELPA

`
M-x package-install [RET] docker-cli [RET]
`

Manually package can be installed by putting `docker-cli.el` somewhere
on the load path and requiring it in `init.el`

`
(require 'docker-cli)
`

## Extending ##

If you need to add new commands to execute in Docker containers just
add new element to the `docker-cli-commands-alist` list. For details
view documentation for the list. In short you will have to define
command that will be executed. If command needs additional parameters
they can be composed through `:arguments-compose-func` part of list
element and regular expression for comint prompt handling (view comint
mode for details).

```
(defun railsc-arguments () '("exec" "rails" "c"))

;; This part was taken from inf-ruby
(defconst inf-ruby-prompt-format
  (concat
   (mapconcat
    #'identity
    '("\\(^%s> *\\)"                      ; Simple
      "\\(^(rdb:1) *\\)"                  ; Debugger
      "\\(^(byebug) *\\)"                 ; byebug
      "\\(^\\(irb([^)]+)"                 ; IRB default
      "\\([[0-9]+] \\)?[Pp]ry ?([^)]+)"   ; Pry
      "\\(jruby-\\|JRUBY-\\)?[1-9]\\.[0-9]\\(\\.[0-9]+\\)*\\(-?p?[0-9]+\\)?" ; RVM
      "^rbx-head\\)")                     ; RVM continued
    "\\|")
   ;; Statement and nesting counters, common to the last four.
   " ?[0-9:]* ?%s *\\)")
  "Format string for the prompt regexp pattern.
Two placeholders: first char in the Simple prompt, and the last
graphical char in all other prompts.")

(setq docker-cli-commands-alist
      (append docker-cli-commands-alist '((rails-console
                                           :command "./bin/rails"
                                           :arguments-compose-func railsc-arguments
                                           :prompt-regexp (format inf-ruby-prompt-format "[?>]" "[\]>*\"'/`]")
                                           :prompt-cont-regexp (format inf-ruby-prompt-format "[?>]" "[\]>*\"'/`]")))))
```
