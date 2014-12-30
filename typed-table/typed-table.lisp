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

(in-package :cl-ana.typed-table)

(declaim (optimize (speed 3)
                   (safety 0)
                   (compilation-speed 0)
                   (debug 3)))

(defun gethash-keywordify (key hash-table)
  (gethash (keywordify (string key)) hash-table))

(defun (setf gethash-keywordify) (value key hash-table)
  (setf (gethash (keywordify (string key)) hash-table)
        value))

(defclass typed-table (table)
  ((field-specs
    :initarg :field-specs
    :initform ()
    ;; renamed for intuitive-ness:
    :accessor table-field-specs
    :documentation "list of typespecs, one per field")
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
    :documentation "Hash table which maps the field symbols to the
    lisp->c converter function for corresponding field.")
   (c->lisp-converter-map
    :initarg :c->lisp-converter-map
    :initform nil
    :accessor typed-table-c->lisp-converter-map
    :documentation "Hash table which maps the field symbols to the
    c->lisp converter function for corresponding field.")))

(defmethod initialize-instance :after ((table typed-table) &key)
  (with-accessors ((field-specs table-field-specs)
                   (lisp->c-converter-map
                    typed-table-lisp->c-converter-map)
                   (c->lisp-converter-map
                    typed-table-c->lisp-converter-map))
      table
    (setf lisp->c-converter-map (make-hash-table :test 'eq))
    (setf c->lisp-converter-map (make-hash-table :test 'eq))
    (loop
       for s in (table-field-symbols table)
       for cs in field-specs
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

(defmethod table-set-field ((table typed-table) field-symbol value)
  "Method on table-set-field that automatically converts the value
into the appropriate CFFI type for the field given by field-symbol.
Note that this function is still pedantic about which particular
numerical type you are giving it, e.g. float vs. integer.  Use plists
to represent a structure (works for nested as well), and vectors to
represent foreign arrays."
  (with-slots (lisp->c-converter-map
               row-pointer
               row-cstruct)
      table
    (funcall (gethash field-symbol lisp->c-converter-map)
             value
             (foreign-slot-pointer row-pointer
                                   row-cstruct
                                   field-symbol))))

(defmethod table-get-field ((table typed-table) field-symbol)
  "Automatically converts field pointer to lisp value."
  (with-accessors ((c->lisp-converter-map
                    typed-table-c->lisp-converter-map)
                   (row-pointer typed-table-row-pointer)
                   (row-cstruct typed-table-row-cstruct))
      table
    (funcall (gethash field-symbol c->lisp-converter-map)
             (foreign-slot-pointer row-pointer
                                   row-cstruct
                                   field-symbol))))

(defun typespec->field-names (compound-typespec)
  "Returns field names from the compound type designation."
  (if (typespec-compound-p compound-typespec)
      (mapcar #'car (rest compound-typespec))
      (error "Non compound type given to typespec->field-names")))

(defun typespec->field-specs (compound-typespec)
  "Returns field typespecs from the compound type designation."
  (if (typespec-compound-p compound-typespec)
      (mapcar #'cdr (rest compound-typespec))
      (error "Non compound type given to typespec->field-specs")))

(defun typed-table->typespec (table)
  "Creates a typespec from the table"
  (append (list :compound)
	  (zip (table-field-names table)
		   (table-field-specs table))))
