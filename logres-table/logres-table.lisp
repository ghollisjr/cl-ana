(in-package logres-table)

(defun load-project-tables ()
  "Loads physical table targets from stored results in work/ directory
for project.  Should always be run after load-project."
  (loop
     for id being the hash-keys in (target-table)
     for tar being the hash-values in (target-table)
     do (let ((expr (target-expr tar)))
          (when (makeres-table::tab? expr)
            (destructuring-bind (progn
                                  (tab source inits opener &rest body))
                expr
              (format t "Loading table ~a~%"
                      id)
              (let ((opener (eval opener))) ; necessary eval
                (setresfn id (funcall opener :read))))))))

(defun table-target? (id)
  (let ((tar (gethash id (target-table))))
    (and (target-stat tar)
         (or (makeres-table::tab? (target-expr tar))
             (makeres-table::ltab? (target-expr tar))
             (typep (target-val tar) 'table)
             (typep (target-val tar) 'reusable-table)))))
