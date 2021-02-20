;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write data to a
;;; dataset by hyberslabs.  The program first writes integers
;;; in a hyperslab selection to a dataset with dataspace
;;; dimensions of DIM0xDIM1, then closes the file.  Next, it
;;; reopens the file, reads back the data, and outputs it to
;;; the screen.  Finally it reads the data again using a
;;; different hyperslab selection, and outputs the result to
;;; the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_hyper.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_hyper.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 6)
(defparameter *DIM1* 8)


(defun print-data (data)
  (dotimes (i *DIM0*)
    (format t " [")
    (dotimes (j *DIM1*)
      (format t " ~3d" (cffi:mem-aref data :int (h5ex:pos2D *DIM1* i j))))
    (format t "]~%")))


(cffi:with-foreign-objects ((start 'hsize-t 2)
			    (stride 'hsize-t 2)
			    (count 'hsize-t 2)
			    (block 'hsize-t 2)
			    (wdata :int (* *DIM0* *DIM1*))
			    (rdata :int (* *DIM0* *DIM1*)))
  ;; Initialize data to "1", to make it easier to see the selections.
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) 1)))

  ;; Print the data to the screen.
  (format t "Original Data:~%")
  (print-data wdata)

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
		(dset (h5dcreate2 file *DATASET* +H5T-STD-I32BE+ space
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))

	   ;; Define and select the first part of the hyperslab selection.
	   (setf (cffi:mem-aref start 'hsize-t 0) 0
		 (cffi:mem-aref start 'hsize-t 1) 0
		 (cffi:mem-aref stride 'hsize-t 0) 3
		 (cffi:mem-aref stride 'hsize-t 1) 3
		 (cffi:mem-aref count 'hsize-t 0) 2
		 (cffi:mem-aref count 'hsize-t 1) 3
		 (cffi:mem-aref block 'hsize-t 0) 2
		 (cffi:mem-aref block 'hsize-t 1) 2)

	   (h5sselect-hyperslab space :H5S-SELECT-SET start stride count block)

	   ;; Define and select the second part of the hyperslab selection,
	   ;; which is subtracted from the first selection by the use of
	   ;; H5S_SELECT_NOTB
	   (setf (cffi:mem-aref block 'hsize-t 0) 1
		 (cffi:mem-aref block 'hsize-t 1) 1)
	   (h5sselect-hyperslab space :H5S-SELECT-NOTB start stride count block)

	   ;; Write the data to the dataset
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ space +H5P-DEFAULT+ wdata)
	   ;; Close and release resources.
	   (h5ex:close-handles (list dset space)))
      (h5ex:close-handles (list file fapl))))

  ;; Open file and dataset using the default properties.

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
		(space (h5dget-space dset)))

	   ;; Read the data using the default properties
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     rdata)
	   ;; Output the data to the screen.
	   (format t "~%Data as written to disk by hyberslabs:~%")
	   (print-data rdata)

	   ;; Initialize the read array.
	   (dotimes (i *DIM0*)
	     (dotimes (j *DIM1*)
	       (setf (cffi:mem-aref rdata :int (h5ex:pos2D *DIM1* i j)) 0)))

	   ;; Define and select the hyperslab to use for reading.
	   (setf (cffi:mem-aref start 'hsize-t 0) 0
		 (cffi:mem-aref start 'hsize-t 1) 1
		 (cffi:mem-aref stride 'hsize-t 0) 4
		 (cffi:mem-aref stride 'hsize-t 1) 4
		 (cffi:mem-aref count 'hsize-t 0) 2
		 (cffi:mem-aref count 'hsize-t 1) 2
		 (cffi:mem-aref block 'hsize-t 0) 2
		 (cffi:mem-aref block 'hsize-t 1) 3)
	   (h5sselect-hyperslab space :H5S-SELECT-SET start stride count block)

	   ;; Read the data using the previously defined hyperslab.
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ space +H5P-DEFAULT+ rdata)

	   ;; Output the data to the screen.
	   (format t "~%Data as read from disk by hyperslab:~%")
	   (print-data rdata)

	   (h5ex:close-handles (list space dset)))
      (h5ex:close-handles (list file fapl)))))
