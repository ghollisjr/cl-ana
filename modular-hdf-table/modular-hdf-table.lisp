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

(in-package :modular-hdf-table)

(defclass modular-hdf-table (table)
  ((field-table-map
    :initform (make-hash-table :test 'equal)
    :initarg :field-table-map
    :accessor modular-hdf-table-field-table-map
    :documentation "Hash table mapping fields to tables")
   ;; for convenience:
   (column-specs
    :initform nil
    :initarg :column-specs
    :accessor modular-hdf-table-column-specs
    :documentation "List of column typespecs")))

(defun get-field-group (hdf-path field-sym)
  (concatenate 'string
               hdf-path
               "/tables/"
               (string field-sym)))

(defun get-names-group (hdf-path)
  (concatenate 'string
               hdf-path
               "/names"))

;;;; Reading:

(defun open-modular-hdf-table (file hdf-path)
  (let* ((names-table (open-hdf-table file (get-names-group hdf-path)))
         (column-names
          (let ((result ()))
            (do-table (row-index names-table)
                ("name"
                 "length")
              (push (char-vector->string name length) result))
            (nreverse result)))
         (column-symbols
          (mapcar (compose #'keywordify #'lispify)
                  column-names)))
    (table-close names-table)
    (let ((field-table-map
           (make-hash-table :test 'equal))
          (column-specs ()))
      (loop
         for sym in column-symbols
         for name in column-names
         do
           (progn
             (setf (gethash sym field-table-map)
                   (open-hdf-table file (get-field-group hdf-path name)))
             (push (typed-table-column-specs
                    (gethash sym field-table-map))
                   column-specs)))
      (make-instance 'modular-hdf-table
                     :field-table-map field-table-map
                     :column-specs (nreverse column-specs)
                     :column-names column-names))))

(defmethod table-load-next-row ((modular-table modular-hdf-table))
  (every #'identity
         (loop
            for v being the hash-values in (modular-hdf-table-field-table-map
                                            modular-table)
            collecting (table-load-next-row v))))

(defmethod table-get-field ((modular-table modular-hdf-table) field-symbol)
  (table-get-field
   (gethash field-symbol (modular-hdf-table-field-table-map modular-table))
   field-symbol))

;;;; Writing:

(defun create-modular-hdf-table (file hdf-path names-specs)
  (hdf-mkgroup file hdf-path)
  (hdf-mkgroup file (concatenate 'string
                                 hdf-path
                                 "/tables"))
  (let* ((column-names (mapcar #'car names-specs))
         (column-specs (mapcar #'cdr names-specs))
         (column-groups (mapcar
                         (lambda (name)
                           (get-field-group hdf-path name))
                         column-names))
         (field-table-map (make-hash-table :test 'equal))
         (names-table
          (create-hdf-table
           file (get-names-group hdf-path)
           (list (cons "name"
                       (list :array
                             :char
                             (maximum
                              (mapcar #'length column-names))))
                 (cons "length"
                       :int)))))
    (loop
       for name in column-names
       do (progn
            (table-set-field names-table :name name)
            (table-set-field names-table :length (length name))
            (table-commit-row names-table)))
    (table-close names-table)
    (loop
       for name in column-names
       for spec in column-specs
       for group in column-groups
       do (setf (gethash (keywordify (lispify name)) field-table-map)
                (create-hdf-table file group (list (cons name spec)))))
    (make-instance 'modular-hdf-table
                   :column-names column-names
                   :column-specs column-specs
                   :field-table-map field-table-map)))

(defmethod table-set-field ((modular-table modular-hdf-table) field-sym value)
  (table-set-field (gethash field-sym
                            (modular-hdf-table-field-table-map modular-table))
                   field-sym
                   value))

(defmethod table-commit-row ((modular-table modular-hdf-table))
  (loop
     for v being the hash-values in (modular-hdf-table-field-table-map
                                     modular-table)
     do (table-commit-row v)))

;;;; Cleanup:

(defmethod table-close ((modular-table modular-hdf-table))
  (loop
     for v being the hash-values in (modular-hdf-table-field-table-map modular-table)
     do (table-close v)))
