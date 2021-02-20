;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write data to a dataset
;;; using a data transform expression.  The program first
;;; writes integers to a dataset using the transform
;;; expression TRANSFORM, then closes the file.  Next, it
;;; reopens the file, reads back the data without a transform,
;;; and outputs the data to the screen.  Finally it reads the
;;; data using the transform expression RTRANSFORM and outputs
;;; the results to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_transform.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_transform.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)
(defparameter *TRANSFORM* (cffi:foreign-string-alloc "x+1"))
(defparameter *RTRANSFORM* (cffi:foreign-string-alloc "x-1"))


(defun print-data (data)
  (dotimes (i *DIM0*)
    (format t " [")
    (dotimes (j *DIM1*)
      (format t " ~3d" (cffi:mem-aref data :int (h5ex:pos2D *DIM1* i j))))
    (format t "]~%")))


(cffi:with-foreign-objects ((wdata :int (* *DIM0* *DIM1*))
			    (rdata :int (* *DIM0* *DIM1*)))

  ;; initialize data
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) (- (* i j) j))))

  ;; Output the data to the screen.
  (format t "Original Data:~%")
  (print-data wdata)

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
		;; Create the dataset transfer property list
		(dxpl (h5pcreate +H5P-DATASET-XFER+))
		;; Create the dataset using the default properties.
		;; Unfortunately we must save as a native type or the
		;; transform operation will fail.
		(dset (h5dcreate2 file *DATASET* +H5T-NATIVE-INT+ space
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	   ;; Define the transform expression.
	   (h5pset-data-transform dxpl *TRANSFORM*)
	   ;; Write the data to the dataset using the dataset transfer
	   ;; property list.
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ dxpl wdata)
	   ;; Close and release resources.
	   (h5ex:close-handles (list dxpl dset space)))
      (h5ex:close-handles (list file fapl))))

  ;; Open file and dataset using the default properties.

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
	       (dxpl (h5pcreate +H5P-DATASET-XFER+)))
	     ;; Read the data using the default properties
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     rdata)
	   ;; Output the data to the screen.
	   (format t "~%Data as written with transform \"~a\":~%"
		   (cffi:foreign-string-to-lisp *TRANSFORM*))
	   (print-data rdata)

	   (h5pset-data-transform dxpl *RTRANSFORM*)

	   ;; Read the data using the dataset transfer property list.
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ dxpl rdata)

	   ;; Output the data to the screen.
	   (format t
		   "~%Data as written with transform \"~a\" and read with transform \"~a\":~%"
		   (cffi:foreign-string-to-lisp *TRANSFORM*)
		   (cffi:foreign-string-to-lisp *RTRANSFORM*))
	   (print-data rdata)

	   (h5ex:close-handles (list dxpl dset)))
      (h5ex:close-handles (list file fapl)))))

(cffi:foreign-string-free *RTRANSFORM*)
(cffi:foreign-string-free *TRANSFORM*)
