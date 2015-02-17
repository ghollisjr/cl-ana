(defun makeres (&rest args)
  "Executes makeres-propogate! and makeres with arguments in
current slime buffer."
  (interactive)
  (let ((slime-buffer (slime-repl))
        (old-buffer (current-buffer)))
    (switch-to-buffer slime-buffer)
    (end-of-buffer)
    (insert "(makeres-propogate!)\n(makeres)")
    (slime-repl-return)))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key [f5]
                            'makeres)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (local-set-key [f5]
                            'makeres)))
