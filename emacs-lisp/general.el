(defun slime-require-cmd (&rest args)
  "Executes (require 'system)"
  (interactive "sLoad system: ")
  (let ((system
         (car args)))
    (slime-command (format "(require '%s)"
                           system))))

(defun slime-require-in-package-cmd (&rest args)
  "Executes (require 'system) (in-package system)"
  (interactive "sLoad system: ")
  (let ((system
         (car args)))
    (slime-command (format "(require '%s) (in-package :%s)"
                           system system))))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key [f6]
                            'slime-require-cmd)
             (local-set-key (kbd "C-<f6>")
                            'slime-require-in-package-cmd)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (local-set-key [f6]
                            'slime-require-cmd)
             (local-set-key (kbd "C-<f6>")
                            'slime-require-in-package-cmd)))
