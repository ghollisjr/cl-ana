(defun slime-require-cmd (&rest args)
  "Executes (require 'system) (in-package system)"
  (interactive "sLoad system: ")
  (let ((system
         (first args)))
    (slime-command (format "(require '%s) (in-package :%s)"
                           system system))))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key [f6]
                            'slime-require-cmd)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (local-set-key [f6]
                            'slime-require-cmd)))
