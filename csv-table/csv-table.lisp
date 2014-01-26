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
;;;; csv-table.lisp

(in-package :csv-table)

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
    :documentation "hash table mapping column-symbols to values")
   (column-symbols
    :initarg :column-symbols
    :initform ()
    :accessor csv-table-column-symbols
    :documentation "Storing the lispified column symbols for
    efficiency.")))

(defun open-csv-table (filename &key
                                  (delimeter #\,)
                                  column-names)
  "Open a CSV file to be read as a table.  Assumes that the first row
  consists of the column names and are separated by the delimeter
  unless column-names keyword argument is given."
  (let* ((file (open filename :direction :input))
	 (row (make-hash-table :test #'equal))
	 (table
          (make-instance 'csv-table
                         :column-names
                         (if column-names
                             column-names
                             (first (read-csv (read-line file)
                                              :separator delimeter)))
                         :file file
                         :delimeter delimeter
                         :row row)))
    (setf (csv-table-column-symbols table)
	  (table-column-symbols table))
    table))

;;; Reading methods:

(defun smart-read-from-string (s)
  (flet ((string-contains (s char)
           (loop
              for c across s
              when (equal c char)
              do (return t)
              finally (return nil))))
    (if (string-contains s #\Space)
        s
        (read-from-string s))))

(defmethod table-load-next-row ((table csv-table))
  (with-accessors ((file csv-table-file)
		   (column-symbols csv-table-column-symbols)
		   (delimeter csv-table-delimeter)
		   (row csv-table-row))
      table
    (let* ((line (read-line file nil nil))
	   (csv-data (when line
		       (mapcar #'smart-read-from-string
			       (first (read-csv line :separator delimeter))))))
      (when line
	(iter (for s in column-symbols)
	      (for v in csv-data)
	      (setf (gethash s row) v))
	t))))

(defmethod table-get-field ((table csv-table) column-symbol)
  (with-accessors ((row csv-table-row))
      table
    (gethash column-symbol row)))

;;; Writing functions:

(defun make-csv-table (filename column-names &optional (delimeter #\,))
  "Creates a CSV file to be written to as a table."
  (let* ((file (open filename
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create))
	 (row (make-hash-table :test #'equal))
	 (table (make-instance 'csv-table
			       :column-names column-names
			       :file file
			       :delimeter delimeter
			       :row row)))
    (setf (csv-table-column-symbols table)
	  (table-column-symbols table))
    (iter (for i upfrom 0)
	  (for n in column-names)
	  (when (not (= i 0))
	    (format file "~a" delimeter))
	  (format file "~a" n))
    (format file "~%")
    table))

(defmethod table-set-field ((table csv-table) column-symbol value)
  (with-accessors ((row csv-table-row))
      table
    (setf (gethash column-symbol row) value)))

(defmethod table-commit-row ((table csv-table))
  (with-accessors ((file csv-table-file)
		   (row csv-table-row)
		   (column-symbols csv-table-column-symbols)
		   (delimeter csv-table-delimeter))
      table
    (iter (for i upfrom 0)
	  (for (key value) in-hashtable row)
	  (when (not (= i 0))
	    (format file "~a" delimeter))
	  (format file "~a" value))
    (format file "~%")))

(defmethod table-close ((table csv-table))
  (with-accessors ((file csv-table-file))
      table
    (close file)))
