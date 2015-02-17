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

(in-package :cl-ana.csv-table)

(defclass csv-table (table)
  ((file
    :initarg :file
    :initform nil
    :accessor csv-table-file
    :documentation "The CSV file.")
   (delimeter
    :initarg :delimeter
    :initform #\,
    :accessor csv-table-delimeter
    :documentation "The delimeter denoting a new field; defaults to a
    comma.")
   (row
    :initarg :row
    :initform nil
    :accessor csv-table-row
    :documentation "hash table mapping field-symbols to values")
   (field-symbols
    :initarg :field-symbols
    :initform ()
    :accessor csv-table-field-symbols
    :documentation "Storing the lispified field symbols for
    efficiency.")
   (read-from-string
    :initarg :read-from-string
    :initform nil
    :accessor csv-table-read-from-string
    :documentation "If nil, values will be read from the CSV as
    strings and not interpreted; if non-nil, table-load-next-row will
    attempt to read a Lisp value from each field.")))

(defun open-csv-table (filename &key
                                  (read-from-string nil)
                                  (delimeter #\,)
                                  field-names)
  "Open a CSV file to be read as a table.  Assumes that the first row
  consists of the field names and are separated by the delimeter
  unless field-names keyword argument is given."
  (let* ((file (open filename :direction :input))
	 (row (make-hash-table :test #'equal))
	 (table
          (make-instance 'csv-table
                         :field-names
                         (if field-names
                             field-names
                             (first (read-csv (read-line file)
                                              :separator delimeter)))
                         :file file
                         :delimeter delimeter
                         :row row
                         :read-from-string read-from-string
                         :access-mode :read)))
    (setf (csv-table-field-symbols table)
	  (table-field-symbols table))
    table))

;;; Reading methods:

(defun smart-read-from-string (s)
  (multiple-value-bind (read-value read-index)
      (read-from-string s)
    (if (equal (length s)
               read-index)
        read-value
        s)))

(defmethod table-load-next-row ((table csv-table))
  (with-accessors ((file csv-table-file)
		   (field-symbols csv-table-field-symbols)
		   (delimeter csv-table-delimeter)
		   (row csv-table-row)
                   (read-from-string csv-table-read-from-string))
      table
    (let* ((reader (if read-from-string
                       #'smart-read-from-string
                       #'identity))
           (line (read-line file nil nil))
	   (csv-data (when line
		       (mapcar reader
			       (first (read-csv line :separator delimeter))))))
      (when line
	(iter (for s in field-symbols)
	      (for v in csv-data)
	      (setf (gethash s row) v))
	t))))

(defmethod table-get-field ((table csv-table) field-symbol)
  (with-accessors ((row csv-table-row))
      table
    (gethash field-symbol row)))

;;; Writing functions:

(defun create-csv-table (filename field-names &optional (delimeter #\,))
  "Creates a CSV file to be written to as a table."
  (let ((*print-pretty* nil))
    (let* ((file (open filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create))
           (row (make-hash-table :test #'equal))
           (table (make-instance 'csv-table
                                 :field-names field-names
                                 :file file
                                 :delimeter delimeter
                                 :row row
                                 :access-mode :write)))
      (setf (csv-table-field-symbols table)
            (table-field-symbols table))
      (iter (for i upfrom 0)
            (for n in field-names)
            (when (not (= i 0))
              (format file "~a" delimeter))
            (format file "~a" n))
      (format file "~%")
      table)))

(defmethod table-set-field ((table csv-table) field-symbol value)
  (with-accessors ((row csv-table-row))
      table
    (setf (gethash field-symbol row) value)))

(defmethod table-commit-row ((table csv-table))
  (let ((*print-pretty* nil))
    (with-accessors ((file csv-table-file)
                     (row csv-table-row)
                     (field-symbols csv-table-field-symbols)
                     (delimeter csv-table-delimeter))
        table
      (loop
         for i upfrom 0
         for key in field-symbols
         do (progn
              (when (not (= i 0))
                (format file "~a" delimeter))
              (format file "~a" (gethash key row))))
      (format file "~%"))))

(defmethod table-close ((table csv-table))
  (with-accessors ((file csv-table-file))
      table
    (close file)))
