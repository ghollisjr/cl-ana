(in-package :makeres-tabletrans)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tabletrans (target-table)
    (makeres::pipe-functions (list #'pass-collapse
                                   #'pass-merge)
                             target-table)))
