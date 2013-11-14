;;;; table.lisp

;;;; I have to make a few changes to the table type if I want to
;;;; include ntuple tables as implemented via GSL.  The ntuples are
;;;; not aware of the number of rows they contain, so a while loop
;;;; would be more appropriate as the general table looping facility,
;;;; which means that dotable needs to be changed to facilitate this
;;;; along with a new generic function being defined on the table
;;;; type: table-end-p.  This function will return nil if there is
;;;; more data to be read (more rows) and t if the row previously read
;;;; was the last one in the table.
;;;;
;;;; This also means I need to make changes to the table types already
;;;; implemented assuming that a table could always be randomly
;;;; accessed.  There should be a table subtype called
;;;; random-access-table which provides the random-access functions
;;;; already implemented by the table types.

;;;; Some thoughts on the even higher-level interface: I could create
;;;; a heterogeneous table type which would contain a list of row
;;;; blocks, each row block being a list of tables for the rows in the
;;;; block.  This allows me to extend a table by adding rows or
;;;; columns to an existing table.  The table-get-field could be
;;;; facilitated via 2 hash tables: one from row-index to column block
;;;; one from symbols to table index; from there just calling
;;;; table-get-field function after accessing the appropriate table
;;;; from the appropriate row block.

(in-package :table)

(defclass table ()
  ((column-names
    :initarg :column-names
    :initform ()
    :accessor table-column-names
    :documentation "List of column names.  Make sure names do not
    clash when lispified.")))

(defun table-column-symbols (table)
  (let* ((column-names (table-column-names table))
	 (lispified-names (mapcar #'lispify column-names)))
    (mapcar #'intern lispified-names)))

(defgeneric table-load-next-row (table)
  (:documentation "Loads the next row into the current row buffer.
  Returns nil if the next row does not exist/there is a read
  failure (which can be the same thing), non-nil otherwise."))

(defgeneric table-get-field (table column-symbol)
  (:documentation "Gets the field datum from the current row for
  column denoted by the column-symbol"))

;; writing is done in a stateful manner, where you access the current
;; table row for writing and then tell the table to commit/write the
;; row when you're done.
(defgeneric table-set-field (table column-symbol value)
  (:documentation "Sets the field value of the current output row"))

(defgeneric table-commit-row (table)
  (:documentation "Commits (writes) the current output row to the table"))

;; Closing tables, sometimes necessary but always call just in case
(defgeneric table-close (table)
  (:documentation "Close any open files, etc."))

;; default: do nothing
(defmethod table-close (table)
  (call-next-method))

;; Marked symbol
(defun marked-symbol-p (s mark)
  "tests whether symbol is marked or not"
  (let ((symbol-string (string s))
	(mark-string (string mark)))
    (if (equal (subseq symbol-string 0 (length mark-string))
	       mark-string)
	t
	nil)))

(defun collect-marked-symbols (tree mark)
  "Collects the set of all marked symbols within a tree (list)"
  (if (listp tree)
      (let ((marked-sets
	     (apply #'append
		    (mapcar (rcurry #'collect-marked-symbols mark) tree))))
	(nreverse
	 (reduce (flip #'adjoin) marked-sets
		 :initial-value nil)))
      (when (symbolp tree)
	(when (marked-symbol-p tree mark)
	      (list tree)))))

(defun unmark-symbol (s mark)
  "returns unmarked version of symbol"
  (intern (subseq (string s) (length (string mark)))))

;; I'm using the marked symbol approach in the body of dotable, so to
;; reference a column in the table, just prefix the lispified column
;; name with a slash.

(defmacro dotable ((rowvar table &optional (mark "/")) &body body)
  "Macro for iterating over a table.  The mark is to be prefixed in
front of each of the column symbols; these marked symbols will be
bound to the values they have in the table during the loop.  You can
choose the mark so as not to clash with any pre-existing variables.
The rowvar, however, is not marked as you get to choose this
yourself."
  (let* ((marked-column-symbols (collect-marked-symbols body mark))
	 (unmarked-column-symbols
	  (mapcar
	   #'(lambda (x) (unmark-symbol x mark))
	   marked-column-symbols))
	 (marked-symbol-bindings
	  (loop
	     for m in marked-column-symbols
	     for u in unmarked-column-symbols
	     collecting `(,m (table-get-field ,table ',u)))))
    (with-gensyms (read-status)
      `(do ((,read-status (table-load-next-row ,table) (table-load-next-row ,table))
	    (,rowvar 0 (1+ ,rowvar)))
	   ((not ,read-status))
	 (let* ,marked-symbol-bindings
	   ,@body)))))
