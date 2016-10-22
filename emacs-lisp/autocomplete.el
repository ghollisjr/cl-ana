;; This isn't working at the moment
;;
;; Was trying to follow this tutorial:
;;
;; http://auto-complete.org/doc/manual.html#extend

(defun target-id-strings ()
  (rest
   (slime-eval
    `(swank:eval-and-grab-output
      "(let ((*print-pretty* nil))
        (mapcar (lambda (id)
                  (format nil \"(res ~s)\" id))
                (target-ids)))"))))

(ac-define-source cl-ana-target-table-source
  '((candidates . target-id-strings)
    (prefix . "(res .*")))

(setq ac-sources '(ac-source-cl-ana-target-table-source))
