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

;; Old version, defunct
;; (defun load-project (&rest args)
;;   (interactive "sLoad version (default is current): \ncWork files need loading (y/n)? ")
;;   (let ((version
;;          (cond
;;           ((null (first args))
;;            "current")
;;           ((equal (first args) "")
;;            "current")
;;           (t
;;            (first args))))
;;         (work-p
;;          (cond
;;           ((eq (second args) ?y)
;;            t)
;;           ((eq (second args) ?n)
;;            nil)
;;           (t t))))
;;     (slime-command "(require 'phd)\n(in-package :phd)\n(load-ana "
;;                    (format "%S" version)
;;                    " "
;;                    (format "%S" work-p)
;;                    ")")))

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
