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

(in-package :cl-ana.ntuple-table)

(defclass ntuple-table (typed-table)
  ((ntuple
    :initarg :ntuple
    :initform nil
    :accessor ntuple-table-ntuple
    :documentation "GSL handler for ntuple object")))

;;; Writing functions:

(defun create-ntuple-table (filename names-specs)
  "Creates an ntuple-table with file located at the path corresponding
to filename and names-specs being an alist mapping the field names to
their typespecs."
  (let* ((typespec (cons :compound names-specs))
	 (cstruct (typespec->cffi-type typespec))
	 (field-names (mapcar #'car names-specs))
	 (field-specs (mapcar #'cdr names-specs))
	 (row-pointer (typespec-foreign-alloc typespec)))
    (let ((ntuple (gsll:create-ntuple filename row-pointer cstruct)))
      (make-instance 'ntuple-table
		     :field-names field-names
		     :field-specs field-specs
		     :ntuple ntuple
		     :row-cstruct cstruct
		     :row-pointer row-pointer))))

;; (defmethod table-set-field ((table ntuple-table) field-symbol value)
;;   (with-accessors ((row typed-table-row-pointer)
;; 		   (cstruct ntuple-table-cstruct))
;;       table
;;     (setf
;;      (foreign-slot-value row
;; 			 cstruct
;; 			 field-symbol)
;;      value)))

(defmethod table-commit-row ((table ntuple-table))
  (with-accessors ((row typed-table-row-pointer)
		   (ntuple ntuple-table-ntuple))
      table
    (gsll:write-ntuple ntuple)))

;;; Reading functions:

(defun open-ntuple-table (filename names-specs)
  "Opens a pre-existing ntuple data file.  Must already know the
typespecs of each of the field types (and names); this is a
limitation of the ntuple file format itself."
  (let* ((typespec (cons :compound names-specs))
	 (cstruct (typespec->cffi-type typespec))
	 (field-names (mapcar #'car names-specs))
	 (field-specs (mapcar #'cdr names-specs))
	 (row (typespec-foreign-alloc typespec)))
    (let ((ntuple (gsll:open-ntuple filename row cstruct)))
      (make-instance 'ntuple-table
		     :field-names field-names
		     :field-specs field-specs
		     :ntuple ntuple
		     :row-cstruct cstruct
		     :row-pointer row))))

(defmethod table-load-next-row ((table ntuple-table))
  (with-accessors ((ntuple ntuple-table-ntuple))
      table
    (let ((read-status (cl-ana.gsl-cffi:gsl-ntuple-read ntuple)))
      (not (equal read-status cl-ana.gsl-cffi:+GSL-EOF+)))))

;; (defmethod table-get-field ((table ntuple-table) field-symbol)
;;   (with-accessors ((row typed-table-row-pointer)
;; 		   (cstruct ntuple-table-cstruct))
;;       table
;;     (foreign-slot-value row
;; 			cstruct
;; 			field-symbol)))

;;; Cleanup:

(defmethod table-close ((table ntuple-table))
  (with-accessors ((ntuple ntuple-table-ntuple)
		   (row typed-table-row-pointer))
      table
    (gsll:close-ntuple ntuple)
    (foreign-free row)))
