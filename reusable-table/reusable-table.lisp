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
;;;; reusable-table.lisp

;;;; reusable-table provides a wrapper macro which, when given the
;;;; form you would have evaluated to open your table for reading,
;;;; creates a closure which will be called whenever the table needs
;;;; reloading.  The table object returned only defines minimal
;;;; interface, but this is a minor inconvenience as it is still
;;;; possible to access the raw table directly.
;;;;
;;;; It can safely be used with do-table since table-load-next-row
;;;; still returns nil when appropriate.

(in-package :cl-ana.reusable-table)

(defclass reusable-table ()
  ((creation-fn
    :initarg :creation-fn
    :initform nil
    :accessor reusable-table-creation-fn
    :documentation "Function which creates the table being wrapped.")
   (creation-fn-form
    :initarg :opener-form
    :initform nil
    :accessor reusable-table-opener-form
    :documentation "Lisp form which returns the table opener function
    when evaluated.  A table opener An optional field which assists in serializing
    reusable tables.")
   (raw-table
    :initarg :raw-table
    :initform nil
    :accessor internal-table
    :documentation "The table which is being wrapped.")
   (needs-reloading
    :initform nil
    :accessor reusable-table-needs-reloading
    :documentation "Boolean which tells the wrapper when it needs to
    close and re-open the table.")))

(defun make-reusable-table (creation-fn &optional opener-form)
  "Returns result of creation-fn if reusable-table, else returns
reusable-table which will make use of creation-fn to generate new
copies when needed."
  (let ((tab (funcall creation-fn)))
    (if (typep tab 'reusable-table)
        tab
        (make-instance 'reusable-table
                       :creation-fn creation-fn
                       :opener-form opener-form
                       :raw-table tab))))
        
(defmacro wrap-for-reuse (table-creation-form &optional opener-form)
  "Creates a reusable (when table-creation-from does not return a
reusable-table) table which places the table-creation-form into a
closure which will be evaluated each time the table gets re-read from
the beginning.  If the creation form returns a reusable-table, simply
returns the table."
  (let ((lambda-form
         `(lambda () ,table-creation-form)))
    `(make-reusable-table ,lambda-form
                          ,opener-form)))

(defmethod table-load-next-row ((table reusable-table))
  (with-slots (raw-table creation-fn needs-reloading)
      table
    (when needs-reloading
      (when (table-open-p raw-table)
        (table-close raw-table))
      (setf raw-table (funcall creation-fn))
      (setf needs-reloading nil))
    (let ((status (table-load-next-row raw-table)))
      (when (not status)
        (setf needs-reloading t))
      status)))

(defmethod table-get-field ((table reusable-table) field-symbol)
  (with-slots (raw-table)
      table
    (table-get-field raw-table field-symbol)))

(defmethod table-field-names ((table reusable-table))
  (with-slots (raw-table)
      table
    (table-field-names raw-table)))

;;; Cleanup:

(defmethod table-close ((table reusable-table))
  "Closes the physical table; note that this is unsafe unless you know
the table needs closing."
  (with-slots (raw-table needs-reloading)
      table
    (when (not needs-reloading)
      (setf needs-reloading t))
    (table-close raw-table)))

;; table-nrows method:
(defmethod table-nrows ((table reusable-table))
  (table-nrows (internal-table table)))

;; access mode:
(defmethod table-access-mode ((table reusable-table))
  (table-access-mode (internal-table table)))
