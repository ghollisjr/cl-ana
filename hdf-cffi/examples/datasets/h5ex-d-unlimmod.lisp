;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to create and extend an unlimited
;;; dataset.  The program first writes integers to a dataset
;;; with dataspace dimensions of DIM0xDIM1, then closes the
;;; file.  Next, it reopens the file, reads back the data,
;;; outputs it to the screen, extends the dataset, and writes
;;; new data to the entire extended dataset.  Finally it
;;; reopens the file again, reads back the data, and utputs it
;;; to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_unlimmod.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_unlimmod.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)
(defparameter *EDIM0* 6)
(defparameter *EDIM1* 10)
(defparameter *CHUNK0* 4)
(defparameter *CHUNK1* 4)


(defun print-data (data rows cols)
  (dotimes (i rows)
    (format t " [")
    (dotimes (j cols)
      (format t " ~3d" (cffi:mem-aref data :int (h5ex:pos2D cols i j))))
    (format t "]~%")))


(cffi:with-foreign-objects ((dims 'hsize-t 2)
			    (extdims 'hsize-t 2)
			    (chunk 'hsize-t 2)
			    (wdata :int (* *DIM0* *DIM1*))
			    (wdata2 :int (* *EDIM0* *EDIM1*)))
  ;; initialize data
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) (- (* i j) j))))

  ;; Create a new file using the default properties.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)
                                                     `(,+H5S-UNLIMITED+
                                                       ,+H5S-UNLIMITED+)))
		;; Create the dataset creation property list, and set the chunk
		;; size.
		(dcpl (let ((tmp (h5pcreate +H5P-DATASET-CREATE+)))
			(setf (cffi:mem-aref chunk 'hsize-t 0) *CHUNK0*
			      (cffi:mem-aref chunk 'hsize-t 1) *CHUNK1*)
			(h5pset-chunk tmp 2 chunk)
			tmp))
		;; Create the unlimited dataset.
		(dset (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ space
				  +H5P-DEFAULT+ dcpl +H5P-DEFAULT+)))
	   ;; Write the data to the dataset.
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     wdata)

	   ;; Close and release resources.
	   (h5ex:close-handles (list dcpl dset space)))
      (h5ex:close-handles (list file fapl))))

  ;; In this next section we read back the data, extend the dataset,
  ;; and write new data to the entire dataset.

  ;; Open file and dataset using the default properties.

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2
		   (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDWR+ fapl))))
    (unwind-protect
	 (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
		(space (let ((tmp (h5dget-space dset)))
                         (h5sget-simple-extent-dims tmp dims +NULL+)
                         tmp))
		(dims[0] (cffi:mem-aref dims 'hsize-t 0))
		(dims[1] (cffi:mem-aref dims 'hsize-t 1)))

	   ;; Allocate space for integer data.
	   (cffi:with-foreign-object (rdata :int (* dims[0] dims[1]))

	     ;; Read the data using the default properties
	     (h5dread dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		      rdata)
	     ;; Output the data to the screen.
	     (format t "Dataset before extension:~%")
	     (print-data rdata dims[0] dims[1]))

	   ;; Extend the dataset.
	   (setf (cffi:mem-aref extdims 'hsize-t 0) *EDIM0*
		 (cffi:mem-aref extdims 'hsize-t 1) *EDIM1*)
	   (h5dset-extent dset extdims)

	   ;; Initialize data for writing to the extended dataset.
	   (dotimes (i *EDIM0*)
	     (dotimes (j *EDIM1*)
	       (setf (cffi:mem-aref wdata2 :int (h5ex:pos2D *EDIM1* i j)) j)))

	   ;; Write the data to the extended dataset.
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     wdata2)

	   ;; Close and release resources.
	   (h5ex:close-handles (list space dset)))
      (h5ex:close-handles (list file fapl))))

  ;; Now we simply read back the data and output it to the screen.
  
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
		(space (let ((tmp (h5dget-space dset)))
                         (h5sget-simple-extent-dims tmp dims +NULL+)
                         tmp))
		(dims[0] (cffi:mem-aref dims 'hsize-t 0))
		(dims[1] (cffi:mem-aref dims 'hsize-t 1)))

	   ;; Get dataspace and allocate memory for the read buffer as before.
	   (cffi:with-foreign-object (rdata :int (* dims[0] dims[1]))
	     ;; Read the data using the default properties
	     (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		       rdata)
	     ;; Output the data to the screen.
	     (format t "~%Dataset after extension:~%")
	     (print-data rdata *EDIM0* *EDIM1*))

	   ;; Close and release resources.
	   (h5ex:close-handles (list space dset)))
      (h5ex:close-handles (list file fapl)))))
