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

(in-package :cl-ana.table)

;;;; plist-table: A table created in-memory from a list of plists.
;;;; Not meant for high-performance computing, but can be useful.
;;;;
;;;; With this theme, it is a read-only table type since writing would
;;;; be done by first converting it into another object and writing it
;;;; however you wish.

(defclass plist-table (table)
  ((plists
    :initarg :plists
    :initform nil
    :accessor plist-table-plists
    :documentation "The plists containing the data.")
   (current-table-index
    :initarg :current-table-index
    :initform -1
    :accessor plist-table-current-table-index
    :documentation "Index to the current table in the plist array.")))

;;; Reading functions:

(defun open-plist-table (plists)
  (make-instance 'plist-table
		 :plists (coerce plists 'vector)
		 :field-names (mapcar (compose #'lispify #'string)
                                      (every-nth (first plists) 2))
                 :access-mode :read))

(defmethod table-load-next-row ((table plist-table))
  (with-accessors ((plists plist-table-plists)
		   (current-table-index plist-table-current-table-index))
      table
    (if (< current-table-index (1- (length plists)))
	(incf current-table-index)
	nil)))

(defmethod table-get-field ((table plist-table) field-symbol)
  (with-accessors ((current-table-index plist-table-current-table-index)
		   (plists plist-table-plists))
      table
    (getf (elt plists current-table-index) field-symbol)))

(defmethod table-close ((table plist-table))
  (when (eq (table-access-mode table) :write)
    (setf (plist-table-plists table)
          (reverse
           (rest (plist-table-plists table)))))
  nil)

;;; Writing functions:

(defun create-plist-table (field-names)
  (make-instance 'plist-table
                 :plists (list (list))
                 :field-names field-names
                 :access-mode :write))

(defmethod table-set-field ((tab plist-table) field-symbol value)
  (with-accessors ((plists plist-table-plists))
      tab
    (setf (getf (first plists) field-symbol)
          value)))

(defmethod table-commit-row ((tab plist-table))
  (with-accessors ((plists plist-table-plists))
      tab
    (push (list) plists)))

;; Misc methods:

(defmethod table-nrows ((tab plist-table))
  (length (plist-table-plists tab)))
