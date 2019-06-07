;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2019 Katherine Cox-Buday
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

(in-package :cl-ana.columnar-table)

(defun pivot (table create-table-fn open-table-fn)
  "Pivot a table so that column values are accessible via rows.

CREATE-TABLE-FN should create the table of type, and open-table-fn
should open the same table again for reuse."
  (check-type table reusable-table)
  (check-type create-table-fn function)
  (check-type open-table-fn function)
  (let ((columnar-table (funcall create-table-fn
                                 '("column-name" "my-values")))
        (field-names (table-field-names table)))
    (loop for field in (table-field-symbols table)
          do (table-set-field columnar-table :|column-name| field)
             (let ((col-vals (make-array 100 :adjustable t :fill-pointer 0)))
               (do-table (rowidx table) ()
                 (vector-push-extend
                  (table-get-field table field)
                  col-vals
                  100))
               (table-set-field columnar-table :|my-values|
                                (adjust-array col-vals (length col-vals))))
             (table-commit-row columnar-table))
    (table-close columnar-table)
    (make-instance 'columnar-table
                   :field-names field-names
                   ;; Fetching rows will involve scanning the
                   ;; underlying table several times.
                   :backing-table (wrap-for-reuse
                                   (funcall open-table-fn columnar-table)))))

(defclass columnar-table (table)
  ((backing-table
    :initarg :backing-table
    :initform (error ":backing-table not specified")
    :accessor backing-table)
   (row
    :initarg :row
    :initform nil
    :accessor columnar-table-row)
   (row-index
    :initform 0
    :accessor row-index)))

(defmethod table-get-field ((table columnar-table) field-symbol)
  (with-accessors ((row columnar-table-row))
      table
    (getf row field-symbol)))


(defmethod table-load-next-row ((table columnar-table))
  (with-accessors ((index row-index)
                   (backing-table backing-table)
                   (row columnar-table-row))
      table
    (incf index)
    (let ((new-row (list)))
      (do-table (rowidx backing-table) ()
        (let* ((column-name (table-get-field backing-table :|column-name|))
               (values-vector (table-get-field backing-table :|my-values|)))
          (when (> index (length values-vector)) (return-from table-load-next-row nil))
          (setf new-row (nconc new-row (list column-name (elt values-vector (- index 1)))))))
      (setf row new-row))
    row))

(defmethod field-values ((table columnar-table) field)
  (with-accessors ((backing-table backing-table))
      table
    (do-table (row-index backing-table) ()
      (when (equalp (table-get-field backing-table :|column-name|)
                    field)
        (table-close backing-table)
        (return-from field-values
          (table-get-field backing-table :|my-values|)))))
  (error "cannot find field ~a" field))
