(in-package :makeres-tabletrans)

;;;; Various compile-time available dynamic variables

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *pass-merge-symmap*
    (make-hash-table :test 'equal))

  (defvar *proj->tab->lfields*
    (make-hash-table :test 'equal)
    "Map from table id to any lfields defined via deflfields.")

  (defvar *proj->tab->ltab-lfields*
    (make-hash-table :test 'equal)
    "Hash table mapping from table-pass target to any lfields needed
due to logical tables having lfields defined for them."))
