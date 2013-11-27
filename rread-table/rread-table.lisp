;;;; rread-table.lisp

(in-package :rread-table)

;;; An rread-table (Random-READ table) is a table that can be read
;;; from at random.  This doesn't guarantee that random reading is
;;; more efficient than sequential reading, but that it is possible.
;;;
;;; To be usable with the table generic functions table-get-field and
;;; table-load-next-row, simply define a method for
;;; rread-table-get-row-field.  The default way to run
;;; table-load-next-row is to just increment the current row index.

(defclass rread-table (table)
  ((read-row-index
    :initarg :read-row-index
    :initform -1
    :accessor rread-table-read-row-index
    :documentation "Index to row which should be sequentually read
    next")
   (nrows
    :initarg :nrows
    :initform nil
    :accessor rread-table-nrows
    :documentation "number of rows in hdf-table")))

(defgeneric rread-table-get-row-field (table row-number column-symbol)
  (:documentation "Reads field column-symbol from row row-number."))

(defmethod table-get-field ((table rread-table) column-symbol)
  (rread-table-get-row-field table
			     (rread-table-read-row-index table)
			     column-symbol))

(defmethod table-load-next-row ((table rread-table))
  (with-accessors ((nrows rread-table-nrows)
		   (read-row-index rread-table-read-row-index))
      table
    (< (incf read-row-index) nrows)))
