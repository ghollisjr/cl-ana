;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example illustrates how to read/write a subset of data (a slab) 
;;; from/to a dataset in an HDF5 file.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_subset.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "subset.h5" *load-pathname*)))
(defparameter *DATASETNAME* "IntArray")
(defparameter *RANK* 2)

(defparameter *DIM0-SUB* 3)
(defparameter *DIM1-SUB* 4)

(defparameter *DIM0* 8)
(defparameter *DIM1* 10)

(cffi:with-foreign-objects
    ((data :int (* *DIM0* *DIM1*))
     (sdata :int (* *DIM0-SUB* *DIM1-SUB*))
     (rdata :int (* *DIM0* *DIM1*))
     (count 'hsize-t 2)
     (offset 'hsize-t 2)
     (stride 'hsize-t 2)
     (block 'hsize-t 2))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((shape  (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
		(dset (h5dcreate2 file *DATASETNAME* +H5T-STD-I32BE+ shape
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	   (dotimes (i *DIM0*)
	       (dotimes (j *DIM1*)
		 (let ((pos (h5ex:pos2D *DIM1* i j)))
		   (if (< j (/ *DIM1* 2))
		       (setf (cffi:mem-aref data :int pos) 1)
		       (setf (cffi:mem-aref data :int pos) 2)))))
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     data)

	   (format t "~%Data written to file:~%")
	   (dotimes (i *DIM0*)
	     (dotimes (j *DIM1*)
	       (format t " ~d" (cffi:mem-aref data :int
					      (h5ex:pos2D *DIM1* i j))))
	     (format t "~%"))

	   (h5ex:close-handles (list dset shape)))
      (h5ex:close-handles (list file fapl))))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDWR+ fapl))))
    (unwind-protect
	 (progn
	   ;; initialize the hyperslab parameters
	   (setf (cffi:mem-aref offset 'hsize-t 0) 1
		 (cffi:mem-aref offset 'hsize-t 1) 2
		 (cffi:mem-aref count 'hsize-t 0) *DIM0-SUB*
		 (cffi:mem-aref count 'hsize-t 1) *DIM1-SUB*
		 (cffi:mem-aref stride 'hsize-t 0) 1
		 (cffi:mem-aref stride 'hsize-t 1) 1
		 (cffi:mem-aref block 'hsize-t 0) 1
		 (cffi:mem-aref block 'hsize-t 1) 1)

	   (let* ((dset (h5dopen2 file *DATASETNAME* +H5P-DEFAULT+))
		  (fshape (h5dget-space dset))
		  (mshape (h5ex:create-simple-dataspace
			   `(,*DIM0-SUB* ,*DIM1-SUB*))))
	     ;; select the hyperslab to be written
	     (h5sselect-hyperslab fshape :H5S-SELECT-SET
				  offset stride count block)
	     (dotimes (i *DIM0-SUB*)
	       (dotimes (j *DIM1-SUB*)
		 (setf (cffi:mem-aref sdata :int (h5ex:pos2D *DIM1-SUB* i j))
		       5)))

	     ;; write to hyperslab selection
	     (h5dwrite dset +H5T-NATIVE-INT+ mshape fshape +H5P-DEFAULT+ sdata)
	     ;; read back the entire dataset
	     (h5dread dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		      rdata)

	     (format t "~%Data in file after subset was written:~%")
	     (dotimes (i *DIM0*)
	       (dotimes (j *DIM1*)
		 (format t " ~d" (cffi:mem-aref rdata :int
						(h5ex:pos2D *DIM1* i j))))
	       (format t "~%"))

	     (h5ex:close-handles (list mshape fshape dset))))
      (h5ex:close-handles (list file fapl)))))
