;;;; table.lisp

;;;; I've been thinking about the way I currently implement dotable,
;;;; and I may in fact change my mind to let the user explicitly state
;;;; which variables should be accessed inside the body of the loop.
;;;; The present method is a bit ugly in that it doesn't take into
;;;; account any lexical scoping issues which would shadow the
;;;; bindings of column variables.

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
    clash when lispified.")
   (access-mode
    :initarg :access-mode
    :initform nil
    :accessor table-access-mode
    :documentation ":write for a writable table, :read for a readable
    table, and :both for a table which has no restriction on being
    written to or read from only.")))

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
  "returns unmarked version of symbol if non-empty string results from
removing the mark, nil otherwise."
  (let ((unmarked-string (subseq (string s)
				 (length (string mark)))))
    (if (not (equal unmarked-string ""))
	(intern unmarked-string)
	nil)))

;; Specified column version: This version requires the user to specify
;; which columns are to be loaded.  The advantage is that the user
;; doesn't have to rely on markings attached to the symbols.
(defmacro do-table ((rowvar table) (&rest column-selections)
		    &body body)
  "Macro for iterating over a table.

rowvar is a symbol which will be bound to the row number inside the
loop body.

table is the table which will be looped upon.

column-selections are a list of 1. column names to access during the
loop, by default the value will be bound to the lispified column name
as a symbol, 2. A list containing a symbol as the first element and
the column name as the second which will be bound to the symbol given
as the first element of the list.

The code body will be run for each row in the table."
  (let* ((selected-column-names
	  (mapcar #'(lambda (x)
		      (if (listp x)
			  (second x)
			  x))
		  column-selections))
	 (bound-column-symbols
	  (mapcar #'(lambda (x)
		      (if (listp x)
			  (first x)
			  (intern (lispify x))))
		  column-selections))
	 (selected-column-symbols
	  (mapcar (compose #'intern #'lispify)
		  selected-column-names))
	 (selected-symbol-bindings
	  (loop
	     for s in bound-column-symbols
	     for c in selected-column-symbols
	     collecting `(,s (table-get-field ,table ',c)))))
    (with-gensyms (read-status)
      `(do ((,read-status (table-load-next-row ,table) (table-load-next-row ,table))
	    (,rowvar 0 (1+ ,rowvar)))
	   ((not ,read-status))
         (declare (integer ,rowvar))
	 (let* ,selected-symbol-bindings
	   ,@body)))))


;; Marked symbol version: I'm using the marked symbol approach in the
;; body of do-table, so to reference a column in the table, just
;; prefix the lispified column name with a slash.
(defmacro do-table-marked ((rowvar table &optional (mark "/")) &body body)
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
