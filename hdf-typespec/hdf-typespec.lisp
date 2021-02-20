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

(in-package :cl-ana.hdf-typespec)

;;; hdf-cffi interface utilities:

(defparameter *hdf-cffi-type-map*
  (list
   (cons hdf5:+H5T-NATIVE-CHAR+ :char)
   (cons hdf5:+H5T-NATIVE-UCHAR+ :uchar)
   (cons hdf5:+H5T-NATIVE-SHORT+ :short)
   (cons hdf5:+H5T-NATIVE-USHORT+ :ushort)
   (cons hdf5:+H5T-NATIVE-INT+ :int)
   (cons hdf5:+H5T-NATIVE-UINT+ :uint)
   (cons hdf5:+H5T-NATIVE-LLONG+ :long-long)
   (cons hdf5:+H5T-NATIVE-ULLONG+ :ullong)
   (cons hdf5:+H5T-NATIVE-FLOAT+ :float)
   (cons hdf5:+H5T-NATIVE-DOUBLE+ :double)))

(defun cffi-native-type (hdf-native-type)
  (cdr (find hdf-native-type *hdf-cffi-type-map*
	     :key #'car
	     :test (lambda (t1 t2)
                     (let ((cmp (hdf5:h5tequal t1 t2)))
                       (if (zerop cmp)
                           nil
                           t))))))

(defun hdf-native-type (cffi-native-type)
  (car (rassoc cffi-native-type *hdf-cffi-type-map*)))

;; defines the structure as an hdf type if necessary from the typespec.

(defun-memoized typespec->hdf-type (typespec)
  ;; (defun typespec->hdf-type (typespec)
  (if (listp typespec)
      ;; handle compound and array types
      (case (first typespec)
	;; array typespec: (:array type rank dim-list)
	(:array
	 (let ((type (typespec->hdf-type
                      (typespec-array-element-type typespec)))
	       (rank (typespec-array-rank typespec))
	       (dim-list (typespec-array-dim-list typespec)))
	   (with-foreign-object (dims 'hsize-t rank)
	     (loop
                for d in dim-list
                for i from 0
                do (setf (mem-aref dims 'hsize-t i) d))
	     (hdf5:h5tarray-create2 type rank dims))))
	(:compound
	 (let* ((names (typespec-compound-field-names typespec))
		(specs (typespec-compound-field-specs typespec))
		(hdf-types (mapcar #'typespec->hdf-type specs))
		(slot-symbols
                 (mapcar (compose #'keywordify #'intern #'string)
                         names))
		(cstruct (typespec->cffi-type typespec))
		(offsets (mapcar (lambda (x) (foreign-slot-offset cstruct x))
				 slot-symbols))
		(compound-tid (hdf5:h5tcreate
                               :H5T-COMPOUND (foreign-type-size cstruct))))
	   (loop
	      for name in names
	      for offset in offsets
	      for type in hdf-types
	      do (hdf5:h5tinsert compound-tid name offset type))
	   compound-tid)))
      ;; return the hdf type corresponding to the cffi type:
      (hdf-native-type typespec)))

;; Construct typespec from hdf type:
(defun h5tget-member-name-as-lisp-string (hdf-type i)
  (let* ((name (hdf5:h5tget-member-name hdf-type i))
         (lisp-name (cffi:foreign-string-to-lisp name)))
    (cffi:foreign-free name)
    lisp-name))

(defun-memoized hdf-type->typespec (hdf-type)
  ;; (defun hdf-type->typespec (hdf-type)
  ;; may need cleaning up
  (let ((hdf-class (hdf5:h5tget-class hdf-type)))
    (case hdf-class
      (:H5T-INTEGER
       (cffi-native-type (hdf5:h5tget-native-type hdf-type :H5T-DIR-DEFAULT)))
      (:H5T-FLOAT
       (cffi-native-type (hdf5:h5tget-native-type hdf-type :H5T-DIR-DEFAULT)))
      (:H5T-ARRAY
       (let* ((base-type (hdf5:h5tget-super hdf-type))
	      (native-base-type (hdf5:h5tget-native-type base-type :H5T-DIR-DEFAULT))
	      (array-rank (hdf5:h5tget-array-ndims hdf-type)))
	 (with-foreign-object (array-dims-pointer 'hsize-t array-rank)
	   (hdf5:h5tget-array-dims2 hdf-type array-dims-pointer)
	   (let (array-dims)
	     (dotimes (index array-rank)
	       (push (mem-aref array-dims-pointer 'hsize-t index)
		     array-dims))
	     (append (list :array)
                     (list (cffi-native-type native-base-type))
                     array-dims)))))
      (:H5T-COMPOUND
       (let* ((nmembers (hdf5:h5tget-nmembers hdf-type))
	      (names
	       (loop for i from 0 to (1- nmembers)
                  collecting (h5tget-member-name-as-lisp-string hdf-type i)))
	      (member-typespecs
	       (loop for i from 0 to (1- nmembers)
                  collecting (hdf-type->typespec
                              (hdf5:h5tget-member-type hdf-type i))))
	      (names-specs (zip names member-typespecs)))
	 (cons :compound names-specs))))))
