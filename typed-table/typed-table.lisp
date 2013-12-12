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

(defmethod table-get-field ((table typed-table) column-symbol)
  "Automatically converts field pointer to lisp value."
  (with-accessors ((c->lisp-converter-map
                    typed-table-c->lisp-converter-map)
                   (row-pointer typed-table-row-pointer)
                   (row-cstruct typed-table-row-cstruct))
      table
    ;;(print row-cstruct)
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
