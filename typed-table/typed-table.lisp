;;;; typed-table.lisp

(in-package :typed-table)

(declaim (optimize (speed 3)
                   (safety 0)
                   (compilation-speed 0)
                   (debug 0)))

(defclass typed-table (table)
  ((column-specs
    :initarg :column-specs
    :initform ()
    :accessor typed-table-column-specs
    :documentation "list of typespecs, one per column")))

(defun typespec->column-names (compound-typespec)
  "Returns column names from the compound type designation."
  (if (typespec-compound-p compound-typespec)
      (mapcar #'car (rest compound-typespec))
      (error "Non compound type given to typespec->column-names")))

(defun typespec->column-specs (compound-typespec)
  "Returns column typespecs from the compound type designation."
  (if (typespec-compound-p compound-typespec)
      (mapcar #'cdr (rest compound-typespec))
      (error "Non compound type given to typespec->column-specs")))

(defun typed-table->typespec (table)
  "Creates a typespec from the table"
  (append (list :compound)
	  (zip (table-column-names table)
		   (typed-table-column-specs table))))

(defun keywordify (symbol)
  (intern
   (string symbol)
   (package-name :keyword)))

;; Specified column version:
(defmacro do-typed-table ((rowvar table) (&rest column-selections)
			  &body body)

  "Macro for iterating over a typed-table in the way that do-table
does, except with data automatically converted from foreign types into
LISP types.

rowvar is a symbol which will be bound to the row number inside the
loop body.

table is the table which will be looped upon.

column-selections are a list of 1. column names to access during the
loop, by default the value will be bound to the lispified column name
as a symbol, 2. A list containing a symbol as the first element and
the column name as the second which will be bound to the symbol given
as the first element of the list.

The code body will be run for each row in the table."
  (with-gensyms (read-status column-type-map symbol-specs)
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
	       collecting `(,s (table-get-field ,table ',c))))
	   (quoted-selected-column-symbols
	    (mapcar
	     #'(lambda (x)
		 `(quote ,x))
	     selected-column-symbols))
	   (quoted-bound-column-symbols
	    (mapcar
	     #'(lambda (x)
		 `(quote ,x))
	     bound-column-symbols))
	   (conversion-bindings
	    (loop
	       for s in bound-column-symbols
	       collecting `(,s
			    (convert-from-foreign
			     ,s
			     (gethash ,(typed-table::keywordify s)
				      ,column-type-map))))))
      ;; I've thought about dynamically (with memoization)
      ;; generating structures instead of using a hash table,
      ;; we'll see if there is a performance hit first
      `(let ((,column-type-map (make-hash-table :test 'equal))
	     (,symbol-specs
	      (zip (mapcar #'typed-table::keywordify (table-column-symbols ,table))
		   (typed-table-column-specs ,table))))
	   (loop
	      for m in (list ,@quoted-bound-column-symbols)
	      for u in (list ,@quoted-selected-column-symbols)
	      do (setf (gethash (typed-table::keywordify m) ,column-type-map)
		       (typespec-flatten-arrays
			(typespec->cffi-type
			 (cdr
			  (assoc (typed-table::keywordify u) ,symbol-specs))))))
	   (do ((,read-status
		 (table-load-next-row ,table)
		 (table-load-next-row ,table))
		(,rowvar 0 (1+ ,rowvar)))
	       ((not ,read-status))
	     (let ,selected-symbol-bindings
	       (let ,conversion-bindings
		 ,@body)))))))

;; Marked column version:
(defmacro do-typed-table-marked ((rowvar table &optional (mark "/"))
			  &body body)
  "Macro for providing table loop like dotable, but where column
values are automatically converted into LISP types."
  (with-gensyms (read-status column-type-map symbol-specs)
    (let* ((marked-column-symbols
	    (table::collect-marked-symbols body mark))
	   (unmarked-column-symbols
	    (mapcar
	     #'(lambda (x) (table::unmark-symbol x mark))
	     marked-column-symbols))
	   (marked-symbol-bindings
	    (loop
	       for m in marked-column-symbols
	       for u in unmarked-column-symbols
	       collecting `(,m (table-get-field ,table ',u))))
	   (quoted-unmarked-column-symbols
	    (mapcar
	     #'(lambda (x)
		 `(quote ,x))
	     unmarked-column-symbols))
	   (quoted-marked-column-symbols
	    (mapcar
	     #'(lambda (x)
		 `(quote ,x))
	     marked-column-symbols))
	   (conversion-bindings
	    (loop
	       for m in marked-column-symbols
	       collecting `(,m
			    (convert-from-foreign
			     ,m
			     (gethash ,(typed-table::keywordify m)
				      ,column-type-map))))))
      ;; I've thought about dynamically (with memoization)
      ;; generating structures instead of using a hash table,
      ;; we'll see if there is a performance hit first
      `(let ((,column-type-map (make-hash-table :test 'equal))
	     (,symbol-specs
	      (zip (mapcar #'typed-table::keywordify (table-column-symbols ,table))
		   (typed-table-column-specs ,table))))
	   (loop
	      for m in (list ,@quoted-marked-column-symbols)
	      for u in (list ,@quoted-unmarked-column-symbols)
	      do (setf (gethash (typed-table::keywordify m) ,column-type-map)
		       (typespec-flatten-arrays
			(typespec->cffi-type
			 (cdr
			  (assoc (typed-table::keywordify u) ,symbol-specs))))))
	   (do ((,read-status
		 (table-load-next-row ,table)
		 (table-load-next-row ,table))
		(,rowvar 0 (1+ ,rowvar)))
	       ((not ,read-status))
	     (let ,marked-symbol-bindings
	       (let ,conversion-bindings
		 ,@body)))))))
