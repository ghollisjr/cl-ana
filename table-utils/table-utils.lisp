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

(in-package :cl-ana.table-utils)

(defun table->plists (table &key
                              field-names
                              (reverse-p t))
  "Returns a list of plists containing the field values you specify.
Only do this for tables that will reasonably fit into memory.  If no
field-names are specified, then all fields will be present in the
result.

Note that the resulting plists will occur in the reverse order as they
occur in the table for efficiency."
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
