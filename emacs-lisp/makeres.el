(defun makeres (&rest args)
  "Executes makeres-propogate! and makeres with arguments in
current slime buffer."
  (interactive)
  (slime-command "(makeres-propogate!) (makeres)"))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key [f5]
                            'makeres)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (local-set-key [f5]
                            'makeres)))

(defun load-project (&rest args)
  (interactive)
  (slime-command "(load-project) (makeres)"))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key [f8]
                            'load-project)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (local-set-key [f8]
                            'load-project)))

(defun save-snapshot (&rest args)
  (interactive "sSave snapshot as (no quotes): ")
  (let ((path (first args)))
    (slime-command (format "(save-snapshot %S)" path))))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key [f7]
                            'save-snapshot)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (local-set-key [f7]
                            'save-snapshot)))

;; Target look-up function:
(defun target-lookup (&rest args)
  (interactive)
  (switch-to-buffer (slime-repl))
  (slime-command "(target-ids)")
  ;; Simulate keyboard input:
  (setq unread-command-events
        (listify-key-sequence "\C-\M-r")))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key [f12]
                            'target-lookup)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (local-set-key [f12]
                            'target-lookup)))
