;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to set the space allocation time
;;; for a dataset.  The program first creates two datasets,
;;; one with the default allocation time (late) and one with
;;; early allocation time, and displays whether each has been
;;; allocated and their allocation size.  Next, it writes data
;;; to the datasets, and again displays whether each has been
;;; allocated and their allocation size.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_alloc.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_alloc.h5" *load-pathname*)))
(defparameter *DATASET1* "DS1")
(defparameter *DATASET2* "DS2")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)


(defun print-space-status-et-storage-size (dset name)
  (cffi:with-foreign-object (space-status 'h5d-space-status-t 1)
    (h5dget-space-status dset space-status)
    (format t "Space for ~a has~abeen allocated.~%"
	    name (if (eql :H5D-SPACE-STATUS-ALLOCATED
			  (cffi:mem-ref space-status
					'h5d-space-status-t))
		     " "
		     " not "))
    (format t "Storage size for ~a is: ~a bytes.~%" name
	    (h5dget-storage-size dset))))


(cffi:with-foreign-object (wdata :int (* *DIM0* *DIM1*))
  ;; initialize data
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) (- (* i j) j))))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (format t "Creating datasets...~%")
    (format t "~a has allocation time H5D_ALLOC_TIME_LATE~%"
	    *DATASET1*)
    (format t "~a has allocation time H5D_ALLOC_TIME_EARLY~%~%"
	    *DATASET2*)
    (unwind-protect
         (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
                (dcpl (h5pcreate +H5P-DATASET-CREATE+))
                ;; Create the dataset using the dataset creation property
                ;; list.
                (dset1 (h5dcreate2 file *DATASET1* +H5T-STD-I32LE+ space
                                   +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
                ;; Set the allocation time to "early". This way we
                ;; can be sure that reading from the dataset
                ;; immediately after creation will return the fill value.
                (dset2 (prog2 (h5pset-alloc-time dcpl :H5D-ALLOC-TIME-EARLY)
                           (h5dcreate2 file *DATASET2* +H5T-STD-I32LE+ space
                                       +H5P-DEFAULT+ dcpl +H5P-DEFAULT+))))

           ;; Retrieve and print space status and storage size for dset1.
           (print-space-status-et-storage-size dset1 *DATASET1*)
           ;; Retrieve and print space status and storage size for dset2.
           (print-space-status-et-storage-size dset2 *DATASET2*)

           (format t "~%Writing data...~%~%")

           ;; Write the data to the datasets
           (h5dwrite dset1 +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                     wdata)
           (h5dwrite dset2 +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                     wdata)

           ;; Retrieve and print space status and storage size for dset1.
           (print-space-status-et-storage-size dset1 *DATASET1*)
           ;; Retrieve and print space status and storage size for dset2.
           (print-space-status-et-storage-size dset2 *DATASET2*)

           ;; Close and release resources.
           (h5ex:close-handles (list dset2 dset1 dcpl space)))
      (h5ex:close-handles (list file fapl)))))
