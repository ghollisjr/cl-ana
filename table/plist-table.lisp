;;;; plist-table.lisp

(in-package :table)

;;;; plist-table: A table created in-memory from a list of plists.
;;;; Not meant for high-performance computing, but can be useful.
;;;;
;;;; With this theme, it is a read-only table type since writing would
;;;; be done by first converting it into another object and writing it
;;;; however you wish.

(defclass plist-table (table)
  ((plists
    :initarg :plists
    :initform nil
    :accessor plist-table-plists
    :documentation "The plists containing the data.")
   (current-table-index
    :initarg :current-table-index
    :initform -1
    :accessor plist-table-current-table-index
    :documentation "Index to the current table in the plist array.")))

;;; Reading functions:

(defun open-plist-table (plists)
  (make-instance 'plist-table
		 :plists (concatenate 'vector plists)
		 :column-names (every-nth (first plists) 2)))

(defmethod table-load-next-row ((table plist-table))
  (with-accessors ((plists plist-table-plists)
		   (current-table-index plist-table-current-table-index))
      table
    (if (< current-table-index (1- (length plists)))
	(incf current-table-index)
	nil)))

(defmethod table-get-field ((table plist-table) column-symbol)
  (with-accessors ((current-table-index plist-table-current-table-index)
		   (plists plist-table-plists))
      table
    (getf (elt plists current-table-index) column-symbol)))
