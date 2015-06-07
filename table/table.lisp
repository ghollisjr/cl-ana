;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
;;;;
;;;; This file is part of cl-ana.
;;;;
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com
;;;; table.lisp

;;;; I've been thinking about the way I currently implement dotable,
;;;; and I may in fact change my mind to let the user explicitly state
;;;; which variables should be accessed inside the body of the loop.
;;;; The present method is a bit ugly in that it doesn't take into
;;;; account any lexical scoping issues which would shadow the
;;;; bindings of field variables.

;;;; Some thoughts on the even higher-level interface: I could create
;;;; a heterogeneous table type which would contain a list of row
;;;; blocks, each row block being a list of tables for the rows in the
;;;; block.  This allows me to extend a table by adding rows or
;;;; fields to an existing table.  The table-get-field could be
;;;; facilitated via 2 hash tables: one from row-index to field block
;;;; one from symbols to table index; from there just calling
;;;; table-get-field function after accessing the appropriate table
;;;; from the appropriate row block.

(in-package :cl-ana.table)

(declaim (optimize (speed 2)
                   (safety 1)
                   (compilation-speed 0)
                   (debug 1)))

(defclass table ()
  ((field-names
    :initarg :field-names
    :initform ()
    :accessor table-field-names
    :documentation "List of field names.")
   (access-mode
    :initarg :access-mode
    :initform nil
    :accessor table-access-mode
    :documentation ":write for a writable table, :read for a readable
    table, and :both for a table which has no restriction on being
    written to or read from only.  nil for a table which is not
    open.")))

(defun table-open-p (table)
  (table-access-mode table))

(defun table-field-symbols (table)
  (mapcar (compose #'keywordify #'intern)
          (table-field-names table)))

(defgeneric table-load-next-row (table)
  (:documentation "Loads the next row into the current row buffer.
  Returns nil if the next row does not exist/there is a read
  failure (which can be the same thing), non-nil otherwise."))

(defgeneric table-activate-fields (table field-names)
  (:documentation "Function for optimization based on which fields are
  to be read from the table; default method is to do nothing which
  works when these optimizations do nothing for a particular table
  type.")
  (:method (table field-names)
    nil))

(defgeneric table-get-field (table field-symbol)
  (:documentation "Gets the field datum from the current row for
  field denoted by the field-symbol"))

;; writing is done in a stateful manner, where you access the current
;; table row for writing and then tell the table to commit/write the
;; row when you're done.
(defgeneric table-set-field (table field-symbol value)
  (:documentation "Sets the field value of the current output row"))

(defgeneric table-commit-row (table)
  (:documentation "Commits (writes) the current output row to the table"))

(defmacro table-push-fields (table &body field-specs)
  "Sets fields and commits row of table using field-specs.

Each field-spec is either a symbol which represents both the
field-symbol and the variable storing the field data."
  `(progn
     ,@(loop
          for fs in field-specs
          collecting
            (if (listp fs)
                `(table-set-field ,table ,(keywordify (first fs)) ,(second fs))
                `(table-set-field ,table ,(keywordify fs) ,fs)))
     (table-commit-row ,table)))

;; Closing tables, sometimes necessary but always call just in case
(defgeneric table-close (table)
  (:documentation "Close any open files, etc."))

;; default: do nothing
(defmethod table-close (table)
  (call-next-method))

;; after method for taking care of access mode:
(defmethod table-close :after ((tab table))
  (setf (table-access-mode tab)
        nil))

;; Some tables can tell you about their size:
(defgeneric table-nrows (table)
  (:documentation "When available, returns the number of rows stored
  in the table, otherwise returns nil.")
  (:method (table)
    nil))

;;;; Table processing functions/macros

(defun table-reduce (table fields fn &key
                                       initial-value)
  "table-reduce mimics reduce but with the modification that fn should
take one more argument than the number of fields specified with the
first argument used to store the state as fn is called across the
table.

table-reduce can be used as a functional/non-macro solution to looping
over the table.

fields is a list of field names/symbols.  They will be passed in the
order specified to fn.

fn is function taking the computation state as the first argument and
then each selected field as an argument in the order given in fields;
can use the state argument to collect a list of values for example."
  (table-activate-fields table fields)
  (let ((field-symbols
         (mapcar (compose #'keywordify
                          #'intern
                          #'string)
                 fields)))
    (flet ((get-fields ()
             (loop for f in field-symbols
                collect (table-get-field table f))))
      ;; initial row read:
      (if (table-load-next-row table)
          (do* ((field-vals nil (get-fields))
                (state initial-value (apply fn state field-vals))
                (read-status t
                             (table-load-next-row table)))
               ((not read-status) state))
          initial-value))))

(defmacro do-table ((rowvar table) field-selections
                    &body body)
  "Macro for iterating over a table.

rowvar is a symbol which will be bound to the row number inside the
loop body.  You can optionally use a list (rowtype rowvar) which will
allow for the rowvar to have a type declared for it.

table is the table which will be looped upon.

To select specific fields from the table for reading, specify all the
desired field names in field-selections; if field-selections
is nil then all fields will be read.  Note that it is still more
efficient to specify all desired fields; for two field data this
results in about a 16% running time difference.  Also you must specify
either none or all of the desired fields to work with during the loop
body.

Each field-selection is a list of 1. field names to access during
the loop, by default the value will be bound to the lispified field
name as a symbol, 2. A list containing a symbol as the first element
and the field name as the second which will be bound to the symbol
given as the first element of the list.

The code body will be run for each row in the table.  If fields are
explicitly selected, then you can use declare statements at the
beginning of the loop body; otherwise this is not supported."
  (let* ((selected-field-names
          (mapcar (lambda (x)
                    (if (listp x)
                        (second x)
                        x))
                  field-selections))
         (bound-field-symbols
          (mapcar (lambda (x)
                    (if (listp x)
                        (first x)
                        (intern x)))
                  field-selections)))
    (multiple-value-bind (rowt rowv)
        (if (listp rowvar)
            (values (first rowvar)
                    (second rowvar))
            (values 'integer
                    rowvar))
      (if field-selections
          ;; explicit fields
          (let ((field-keyword-symbols
                 (mapcar (compose #'keywordify
                                  #'string)
                         selected-field-names)))
            (with-gensyms (load-row tab)
              `(let ((,tab ,table))
                 (do ((,rowv 0 (1+ ,rowv))
                      (,load-row (table-load-next-row ,tab)
                                 (table-load-next-row ,tab)))
                     ((not ,load-row) nil)
                   (declare (,rowt ,rowv))
                   (olet ,(loop
                             for b in bound-field-symbols
                             for k in field-keyword-symbols
                             collecting `(,b (table-get-field ,tab ,k)))
                     ,@body)))))
          ;; implicit fields
          (with-gensyms (field-symbols
                         field-names
                         bound-symbols
                         bound
                         xs
                         x)
            `(let* ((,field-names
                     (table-field-names ,table))
                    (,field-symbols
                     (table-field-symbols ,table))
                    (,bound-symbols
                     (mapcar (compose #'intern #'string)
                             ,field-symbols)))
               (table-reduce ,table
                             ,field-names
                             (lambda (,rowv &rest ,xs)
                               (declare (,rowt ,rowv))
                               (loop
                                  for ,bound in ,bound-symbols
                                  for ,x in ,xs
                                  do (set ,bound ,x))
                               ,@body
                               (1+ ,rowv))
                             :initial-value 0)))))))

;; DEFUNCT & DEPRECATED
;;
;; This is kept for comparison with the non-olet version
(defmacro do-table-old ((rowvar table) field-selections
                        &body body)
  "Macro for iterating over a table.

rowvar is a symbol which will be bound to the row number inside the
loop body.  You can optionally use a list (rowtype rowvar) which will
allow for the rowvar to have a type declared for it.

table is the table which will be looped upon.

To select specific fields from the table for reading, specify all the
desired field names in field-selections; if field-selections
is nil then all fields will be read.  Note that it is still more
efficient to specify all desired fields; for two field data this
results in about a 16% running time difference.  Also you must specify
either none or all of the desired fields to work with during the loop
body.

Each field-selection is a list of 1. field names to access during
the loop, by default the value will be bound to the lispified field
name as a symbol, 2. A list containing a symbol as the first element
and the field name as the second which will be bound to the symbol
given as the first element of the list.

The code body will be run for each row in the table.  If fields are
explicitly selected, then you can use declare statements at the
beginning of the loop body; otherwise this is not supported."
  (let* ((selected-field-names
          (mapcar (lambda (x)
                    (if (listp x)
                        (second x)
                        x))
                  field-selections))
         (bound-field-symbols
          (mapcar (lambda (x)
                    (if (listp x)
                        (first x)
                        (intern (lispify x))))
                  field-selections)))
    (multiple-value-bind (rowt rowv)
        (if (listp rowvar)
            (values (first rowvar)
                    (second rowvar))
            (values 'integer
                    rowvar))
      (if field-selections
          `(table-reduce ,table
                         (list ,@selected-field-names)
                         (lambda (,rowv ,@bound-field-symbols)
                           (declare (,rowt ,rowv))
                           ,@body
                           (1+ ,rowv))
                         :initial-value 0)
          (with-gensyms (field-symbols
                         field-names
                         bound-symbols
                         bound
                         xs
                         x)
            `(let* ((,field-names
                     (table-field-names ,table))
                    (,field-symbols
                     (table-field-symbols ,table))
                    (,bound-symbols
                     (mapcar (compose #'intern #'string)
                             ,field-symbols)))
               (table-reduce ,table
                             ,field-names
                             (lambda (,rowv &rest ,xs)
                               (declare (,rowt ,rowv))
                               (loop
                                  for ,bound in ,bound-symbols
                                  for ,x in ,xs
                                  do (set ,bound ,x))
                               ,@body
                               (1+ ,rowv))
                             :initial-value 0)))))))
