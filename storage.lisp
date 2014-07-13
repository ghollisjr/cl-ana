(in-package :makeres-tabletrans)

;;;; Defines two operators, ptab and ltab.
;;;;
;;;; ptab denotes a physical table (storage on disk/in memory) and has
;;;; a lambda list:
;;;;
;;;; (ptab expr read write)
;;;;
;;;; where expr is an expression used to determine how to compute the
;;;; table, read is an expression which will be evaluated to open the
;;;; table for reading, and write is an expression evaluated to open
;;;; the table for writing.  The table objects will be closed as
;;;; appropriate automatically.
;;;;
;;;; ltab denotes a logical table (no storage, always recomputed) and
;;;; has a lambda list
;;;;
;;;; (ltab expr)
;;;;
;;;; where expr is an expression used to determine how to compute the
;;;; table.

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  )
