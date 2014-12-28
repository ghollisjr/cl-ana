(defun makeres (&rest args)
  "Executes makeres with arguments in current slime buffer."
  (interactive)
  (let ((cmd "(makeres)"))
    (slime-write-string cmd)
    (slime-repl-send-string cmd)))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key [f5]
                            'makeres)))
