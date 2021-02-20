;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

(defpackage #:h5ex
  (:documentation "hdf5-examples: Common helpers for hdf5-cffi examples.")
  (:use #:cl)
  (:export close-handle
           close-handles
           create-null-dataspace
           create-scalar-dataspace
           create-simple-dataspace
           create-c-string-type
           create-c-string-type-utf8
           create-f-string-type
           create-f-string-type-utf8
           pos2D))

(in-package :h5ex)

;; handles

(defun close-handle (hnd)
  "Close an HDF5 handle"
  (unless (< 0 (hdf5:h5iis-valid hnd))
    (error "Invalid handle found."))
  (let ((type (hdf5:h5iget-type hnd)))
    (cond ((eql type :H5I-ATTR) (hdf5:h5aclose hnd))
          ((eql type :H5I-DATASET) (hdf5:h5dclose hnd))
          ((eql type :H5I-DATASPACE) (hdf5:h5sclose hnd))
          ((eql type :H5I-DATATYPE) (hdf5:h5tclose hnd))
          ((eql type :H5I-FILE) (hdf5:h5fclose hnd))
          ((eql type :H5I-GENPROP-LST) (hdf5:h5pclose hnd))
          ((eql type :H5I-GROUP) (hdf5:h5gclose hnd))
          (t (error (format nil "Can't close handle. ~a~%" type))))))

(defun close-handles (handles)
  (declare (type list handles))
  "Close a list of handles"
  (dolist (h handles)
    (close-handle h)))

;; dataspaces

(defun create-null-dataspace ()
  "Create a null dataspace"
  (hdf5:h5screate :H5S-NULL))

(defun create-scalar-dataspace ()
  "Create a scalar dataspace"
  (hdf5:h5screate :H5S-SCALAR))

(defun create-simple-dataspace (dims &optional maxdims)
  "Create a simple dataspace"
  
  ;; list arguments expected
  (unless (and (consp dims) (listp maxdims))
    (error "List arguments expected."))

  ;; rank check
  (when (and maxdims
             (not (= (list-length dims) (list-length maxdims))))
    (error "Rank mismatch in simple dataspace definition."))
  (when (or (< (list-length dims) 1)
            (> (list-length dims) 32))
    (error "The dataspace rank must be between 1 and 32."))

  ;; element checks
  (when (some #'(lambda (x) (or (not (integerp x)) (< x 1))) dims)
    (error "The dimensions must be positive integers."))
  (when (and maxdims
             (some #'(lambda (x) (or (not (integerp x))
                                     (and (not (eql x hdf5:+H5S-UNLIMITED+))
                                          (< x 1))))
                   maxdims))
    (error "The maximum dimensions must be positive integers."))
  (when (and maxdims
             (some #'null
                   (mapcar #'(lambda (x y) (if (eql y hdf5:+H5S-UNLIMITED+)
                                               T (<= x y)))
                           dims maxdims)))
    (error "A dimension must not exceed it's corresponding maximum."))

  ;; go
  (let* ((rank (list-length dims))
         (dims-ptr (cffi:foreign-alloc 'hdf5:hsize-t :count rank
                                       :initial-contents dims))
         (maxdims-ptr (if maxdims
                          (cffi:foreign-alloc 'hdf5:hsize-t :count rank
                                              :initial-contents maxdims)
                          hdf5:+NULL+))
         (space (hdf5:h5screate-simple rank dims-ptr maxdims-ptr)))
    (cffi:foreign-free dims-ptr)
    (when maxdims (cffi:foreign-free maxdims-ptr))
    space))

;; common string datatypes

(defun create-c-string-type (&optional length)
  "Create a C-style string datatype"
  (when length
    (unless (and (integerp length) (< 0 length))
      (error "Length must be a positive integer.")))
  
  (let ((result (hdf5:h5tcopy hdf5:+H5T-C-S1+)))
    (hdf5:h5tset-size result (if length length hdf5:+H5T-VARIABLE+))
    result))

(defun create-c-string-type-utf8 (&optional length)
  (let ((result (create-c-string-type length)))
    (hdf5:h5tset-cset result :H5T-CSET-UTF8)
    result))

(defun create-f-string-type (&optional length)
  "Create a FORTRAN-style string datatype"
  (when length
    (unless (and (integerp length) (< 0 length))
      (error "Length must be a positive integer.")))
  
  (let ((result (hdf5:h5tcopy hdf5:+H5T-FORTRAN-S1+)))
    (hdf5:h5tset-size result (if length length hdf5:+H5T-VARIABLE+))
    result))

(defun create-f-string-type-utf8 (&optional length)
  (let ((result (create-f-string-type length)))
    (hdf5:h5tset-cset result :H5T-CSET-UTF8)
    result))

;; array access

(defun pos2D (cols i j)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum cols i j))
  "2D array access function"
  (the fixnum (+ (the fixnum (* cols i)) j)))
