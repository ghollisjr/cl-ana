(in-package makeres-tabletrans)

;;;; Defines gensym tables which allows the reuse of gensyms
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gsym-tables*
    (make-hash-table :test 'equal)
    "Map from id to gsym table")

  ;; structure for storing gsym table info
  (defstruct gsym-table
    symbols
    available-symbols)

  (defun gsym (id)
    (when (not (gethash id *gsym-tables*))
      (setf (gethash id *gsym-tables*)
            (make-gsym-table :symbols (list (gensym))
                             :available-symbols nil)))
    (symbol-macrolet
        ((gsym-tab
          (gethash id *gsym-tables*))
         (available (gsym-table-available-symbols gsym-tab))
         (all (gsym-table-symbols gsym-tab)))
      (if available
          (pop available)
          (let ((gsym (gensym)))
            (push gsym all)
            gsym))))

  (defun clrgsym (id)
    "Frees gsyms for reuse"
    (symbol-macrolet ((gsym-tab (gethash id *gsym-tables*)))
      (when gsym-tab
        (setf (gsym-table-available-symbols gsym-tab)
              (copy-list (gsym-table-symbols gsym-tab)))))
    nil))
