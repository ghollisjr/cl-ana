;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
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
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.table-utils)

(defun table->plists (table &key
                              field-names
                              (reverse-p t))
  "Returns a list of plists containing the field values you specify.
Only do this for tables that will reasonably fit into memory.  If no
field-names are specified, then all fields will be present in the
result.

Note that the resulting plists will occur in the reverse order as they
occur in the table if reverse-p is NIL; this is not the default option
but is more efficient."
  (when (not field-names)
    (setf field-names
          (table-field-names table)))
  (let ((result
         (table-reduce table field-names
                       (lambda (state &rest fields)
                         (push (loop
                                  for fn in field-names
                                  for f in fields
                                  appending (list (keywordify (lispify fn))
                                                  f))
                               state))
                       :initial-value ())))
    (if reverse-p
        (nreverse result)
        result)))

(defun table-row->plist (table field-names)
  "Reads the current row from table and generates a plist containing
the field values of each field-name in the row."
  (loop
     for fn in field-names
     appending (list (keywordify (lispify fn))
                     (table-get-field table fn))))

(defun table-copy (from to &optional fields)
  "Reads all entries from from and writes them to to.  If fields are
specified, only selected fields are used, otherwise all fields are
copied."
  (let* ((field-names
          (if fields
              fields
              (table-field-names from)))
         (field-keysyms
          (mapcar (lambda (x)
                    (keywordify
                     (lispify x)))
                  field-names)))
    (do ((load-state (table-load-next-row from)
                     (table-load-next-row from)))
        ((null load-state) nil)
      (loop
         for k in field-keysyms
         do (table-set-field to
                             k
                             (table-get-field from k)))
      (table-commit-row to))))

(defgeneric table-field-values (table field)
  (:documentation "Collects field values across all rows.")
  (:method (table field)
    (check-type table table)
    (check-type field symbol)
    (let ((field-vals (make-array 100 :adjustable t :fill-pointer 0)))
      (do-table (rowidx table) ()
        (vector-push-extend
         (table-get-field table field)
         field-vals 100))
      (adjust-array field-vals (length field-vals)))))

(defun table-summarize (table)
  "Summarizes information about a table."
  (check-type table table)
  (let* ((fields (table-field-names table))
         (field-symbols (table-field-symbols table))
         (max-col-len (reduce #'max (mapcar #'length fields))))
    (with-output-to-string (summary)
      (let ((non-null-from-field (make-hash-table :test #'equalp))
            (row-count 0))
        (do-table (row-index table) ()
          (incf row-count)
          (loop for field in fields
                for field-sym in field-symbols
                do (let* ((field-info (gethash field non-null-from-field (list)))
                          (field-val (table-get-field table field-sym))
                          (field-type (type-of field-val))
                          (field-type-friendly (cond
                                                 ((stringp field-val)
                                                  'string)
                                                 ((eq field-type 'null) nil)
                                                 (t field-type)))
                          (field-info-types (getf field-info :type)))
                     (when field-val (incf (getf field-info :populated-count 0)))
                     (when field-type-friendly
                       (setf (getf field-info :type)
                             (adjoin field-type-friendly field-info-types :test #'equalp)))
                     (setf (gethash field non-null-from-field) field-info))))
        (format summary "RangeIndex: ~a entries, ~a to ~a~%" row-count -1 -1)
        (format summary "Data columns (total ~a columns):~%" (length fields))
        (let ((field-types (list)))
          (loop for field in fields
                for field-info = (gethash field non-null-from-field)
                for field-info-types = (alexandria:flatten (getf field-info :type))
                do (dolist (type field-info-types) (setf field-types (adjoin type field-types)))
                   (format summary (format nil "~~~aa ~~a populated ~~a~~%" max-col-len)
                           field (getf field-info :populated-count) (getf field-info :type)))
          (format summary "types: ~a" field-types))))))

(defmacro table-value-counts (table field)
  "Counts the number of values present in a table's field."
  `(let ((count-from-val (make-hash-table :test #'equalp)))
     (table-reduce
      ,table (list ,field)
      (lambda (state field)
        (incf (gethash field count-from-val 0))))
     (hash-table->alist count-from-val)))

(defgeneric table-correlation-matrix (table)
  (:documentation
   "Builds a matrix of correlation coefficients between all fields.")
  (:method (table)
    (let* ((fields (table-field-symbols table))
           ;; Memoize calculations
           (mean-from-field (make-hash-table :size (length fields)))
           ;; col -> col -> correlation coefficient
           (correlation-matrix (list)))
      (loop
        for field-index from 0
        for x-name in fields
        for x-correlations = (assoc x-name correlation-matrix)
        ;; x without numerics
        for x = (table-field-values table x-name)
        do (when (every #'numberp x)
             (loop
               with x-hat = (or (gethash x-name mean-from-field)
                                (setf (gethash x-name mean-from-field)
                                      (mean x)))
               for field-index* from field-index
               for y-name in fields
               ;; y without numerics
               for y = (table-field-values table y-name)
               do
                  (when (every #'numberp y)
                    (let* ((y-hat (or (gethash y-name mean-from-field)
                                      (setf (gethash y-name mean-from-field)
                                            (mean y))))
                           (correlation-coefficient
                             (or
                              (cdr (assoc x-name (cdr (assoc y-name correlation-matrix))))
                              (/ (- (sum (* x y))
                                    (* (length x) x-hat y-hat))
                                 (* (sqrt (- (sum (expt x 2))
                                             (* (length x) (expt x-hat 2))))
                                    (sqrt (- (sum (expt y 2))
                                             (* (length y) (expt y-hat 2)))))))))
                      (setf x-correlations
                            (acons y-name correlation-coefficient x-correlations)))))
             (setf correlation-matrix (acons x-name x-correlations correlation-matrix))))
      correlation-matrix)))

(defun table-subset (table indexes)
  "Returns a subset of a table as a plist."
  (let ((field-symbols (table-field-symbols table))
        (rows (list)))
    (do-table (rowidx table) ()
      (when (member rowidx indexes)
        (setf
         rows
         (append
          rows
          (list
           (loop
             for field in field-symbols
             nconc (list (keywordify (lispify field))
                         (table-get-field table field))))))
        ;; Only continue iterating over the table if there are more
        ;; indexes to pluck.
        (when (>= (length rows) (length indexes))
          (return-from table-subset rows))))
    (error "didn't produce the expected number of rows: ~a not ~a"
           (length rows) (length indexes))))
