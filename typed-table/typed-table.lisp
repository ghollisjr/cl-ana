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
    :documentation "list of typespecs, one per column")
   (row-cstruct
    :initarg :row-cstruct
    :initform nil
    :accessor typed-table-row-cstruct
    :documentation "CFFI cstruct type designator for the row object")
   (row-pointer
    :initarg :row-pointer
    :initform nil
    :accessor typed-table-row-pointer
    :documentation "pointer to foreign object storing current row
    information.")
   (lisp->c-converter-map
    :initarg :lisp->c-converter-map
    :initform nil
    :accessor typed-table-lisp->c-converter-map
    :documentation "Hash table which maps the column symbols to the
    lisp->c converter function for corresponding column.")
   (c->lisp-converter-map
    :initarg :c->lisp-converter-map
    :initform nil
    :accessor typed-table-c->lisp-converter-map
    :documentation "Hash table which maps the column symbols to the
    c->lisp converter function for corresponding column.")))

(defmethod initialize-instance :after ((table typed-table) &key)
  (with-accessors ((column-specs typed-table-column-specs)
                   (lisp->c-converter-map
                    typed-table-lisp->c-converter-map)
                   (c->lisp-converter-map
                    typed-table-c->lisp-converter-map))
      table
    (setf lisp->c-converter-map (make-hash-table :test 'equal))
    (setf c->lisp-converter-map (make-hash-table :test 'equal))
    (loop
       for s in (table-column-symbols table)
       for cs in column-specs
       do
         (progn
           (setf (gethash s lisp->c-converter-map)
                 (typespec->lisp-to-c cs))
           (setf (gethash s c->lisp-converter-map)
                 (typespec->c-to-lisp cs))))))

;;; These methods make it so that all you have to do is define methods
;;; on table-load-next-row and table-commit-row which properly set the
;;; row-pointer value to the current row (as well as allocating &
;;; freeing space as appropriate).

(defmethod table-set-field ((table typed-table) column-symbol value)
  "Method on table-set-field that automatically converts the value
into the appropriate CFFI type for the field given by column-symbol.
Note that this function is still pedantic about which particular
numerical type you are giving it, e.g. float vs. integer.  Use plists
to represent a structure (works for nested as well), and vectors to
represent foreign arrays."
  (with-slots (lisp->c-converter-map
               row-pointer
               row-cstruct)
      table
    (funcall (gethash column-symbol lisp->c-converter-map)
             value
             (foreign-slot-pointer row-pointer
                                   row-cstruct
                                   column-symbol))))

(defmethod table-get-field :after ((table typed-table) column-symbol)
  "Automatically converts field pointer to lisp value."
  (with-slots (c->lisp-converter-map
               row-pointer
               row-cstruct)
      table
    (funcall (gethash column-symbol c->lisp-converter-map)
             (foreign-slot-pointer row-pointer
                                   row-cstruct
                                   column-symbol))))

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
		       ;;(typespec-flatten-arrays
			(typespec->cffi-type
			 (cdr
			  (assoc (typed-table::keywordify u)
                                 ,symbol-specs)))
                        ;;)
                       ))
	   (do ((,read-status
		 (table-load-next-row ,table)
		 (table-load-next-row ,table))
		(,rowvar 0 (1+ ,rowvar)))
	       ((not ,read-status))
             (declare (integer ,rowvar))
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
		       ;;(typespec-flatten-arrays
			(typespec->cffi-type
			 (cdr
			  (assoc (typed-table::keywordify u)
                                 ,symbol-specs)))
                        ;;)
                       ))
	   (do ((,read-status
		 (table-load-next-row ,table)
		 (table-load-next-row ,table))
		(,rowvar 0 (1+ ,rowvar)))
	       ((not ,read-status))
             (declare (integer ,rowvar))
	     (let ,marked-symbol-bindings
	       (let ,conversion-bindings
		 ,@body)))))))
