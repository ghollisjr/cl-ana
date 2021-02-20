;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write array datatypes
;;; to a dataset.  The program first writes integers arrays of
;;; dimension ADIM0xADIM1 to a dataset with a dataspace of
;;; DIM0, then closes the  file.  Next, it reopens the file,
;;; reads back the data, and outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_array.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_array.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *ADIM0* 3)
(defparameter *ADIM1* 5)


(defun pos (rows cols i j k)
  "3D array position"
  (+ (* (+ (* i rows) j) cols) k))


(cffi:with-foreign-objects
    ((dims 'hsize-t 1)
     (adims 'hsize-t 2)
     (wdata :int (* *DIM0* *ADIM0* *ADIM1*)))

  (setf (cffi:mem-aref dims 'hsize-t 0) *DIM0*
	(cffi:mem-aref adims 'hsize-t 0) *ADIM0*
	(cffi:mem-aref adims 'hsize-t 1) *ADIM1*)

  ;; Initialize data. i is the element in the dataspace, j and k the
  ;; elements within the array datatype.
  (dotimes (i *DIM0*)
    (dotimes (j *ADIM0*)
      (dotimes (k *ADIM1*)
	(setf (cffi:mem-aref wdata :int (pos *ADIM0* *ADIM1* i j k))
	      (+ (* i j) (- (* j k)) (* i k))))))

  ;; Create a new file using the default properties.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 ;; Create array datatypes for file and memory.
	 (let* ((filetype (h5tarray-create2 +H5T-STD-I64LE+ 2 adims))
		(memtype (h5tarray-create2 +H5T-NATIVE-INT+ 2 adims))
		;; Create dataspace. Setting maximum size to NULL sets the
		;; maximum size to be the current size.
		(space (h5ex:create-simple-dataspace `(,*DIM0*)))
		;; Create the dataset and write the array data to it.
		(dset (h5dcreate2 file *DATASET* filetype space +H5P-DEFAULT+
				  +H5P-DEFAULT+ +H5P-DEFAULT+)))
	   (h5dwrite dset memtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ wdata)

	   ;; Close and release resources.
	   (h5ex:close-handles (list dset space memtype filetype)))
      (h5ex:close-handles (list file fapl))))

  ;; Now we begin the read section of this example.  Here we assume
  ;; the dataset and array have the same name and rank, but can
  ;; have any size.  Therefore we must allocate a new array to read
  ;; in data dynamically.

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
		(filetype (h5dget-type dset))
		(space (h5dget-space dset)))

	   ;; Get the array datatype dimensions.
	   (h5tget-array-dims2 filetype adims)

	   ;; Get dataspace and allocate memory for read buffer.
	   (h5sget-simple-extent-dims space dims +NULL+)

	   ;; Allocate space for integer data.
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0))
		 (adims[0] (cffi:mem-aref adims 'hsize-t 0))
		 (adims[1] (cffi:mem-aref adims 'hsize-t 1))
		 ;; Create the memory datatype.
		 (memtype (h5tarray-create2 +H5T-NATIVE-INT+ 2 adims)))
	     (cffi:with-foreign-object (rdata :int (* dims[0] adims[0]
						      adims[1]))
	       ;; Read the data.
	       (h5dread dset memtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rdata)

	       ;; Output the data to the screen.
	       (dotimes (i *DIM0*)
		 (format t "~a[~a]:~%" *DATASET* i)
		 (dotimes (j *ADIM0*)
		   (format t " [")
		   (dotimes (k *ADIM1*)
		     (format t " ~3d" (cffi:mem-aref rdata :int
						     (pos *ADIM0* *ADIM1*
							  i j k))))
		   (format t "]~%"))
		 (format t "~%"))

	       (h5tclose memtype)))

	   ;; Close and release resources.
	   (h5ex:close-handles (list space filetype dset)))
      (h5ex:close-handles (list file fapl)))))
