;;;; table-chain.lisp

(in-package :table)

;;;; table-chain: A read-only chain of tables of any type; initialized
;;;; by providing code to evaluate which generates the appropriate
;;;; table object whenever it is time to load it.  It is implemented
;;;; using closures.

(defclass table-chain (table)
  ((creation-functions
    :initarg :creation-functions
    :initform ()
    :accessor table-chain-creation-functions
    :documentation "Functions which create each table in the
    chain.")
   (current-table
    :initarg :current-table
    :initform nil
    :accessor table-chain-current-table
    :documentation "Current table being accessed.")
   (current-table-index
    :initarg :current-table-index
    :initform -1
    :accessor table-chain-current-table-index
    :documentation "Index to current table.")))

(defun open-table-chain (creation-functions)
  "Creates a chain of tables by running the appropriate
creation-function when the previous table fails on
table-load-next-row.

Each creation function should be a function taking zero arguments and
returning a table."
  (let ((table
	 (make-instance 'table-chain
			:creation-functions (concatenate 'vector creation-functions))))
    (load-next-table table)
    (setf (table-column-names table)
	  (table-column-names (table-chain-current-table table)))
    table))

(defun reset-table-chain (table-chain)
  "Resets the table counter in the chain so that it can be re-read."
  (setf (table-chain-current-table-index table-chain) -1))

(defun load-next-table (table-chain)
  "Loads the next table in the chain."
  (with-accessors ((current-table table-chain-current-table)
		   (current-table-index table-chain-current-table-index)
		   (creation-functions table-chain-creation-functions))
      table-chain
    (let ((n-tables (length creation-functions)))
      (if (< current-table-index (1- n-tables))
	  (setf current-table
		(funcall (elt creation-functions (incf current-table-index))))
	  nil))))

(defmethod table-load-next-row ((table table-chain))
  (with-accessors ((current-table table-chain-current-table)
		   (current-table-index table-chain-current-table-index)
		   (creation-functions table-chain-creation-functions))
      table
    (if (table-load-next-row current-table)
	t
	(and
	  (load-next-table table)
	  (table-load-next-row table)))))

(defmethod table-get-field ((table table-chain) column-symbol)
  (with-accessors ((current-table table-chain-current-table))
      table
    (table-get-field current-table column-symbol)))
