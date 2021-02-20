;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write bitfield
;;; datatypes to a dataset.  The program first writes bit
;;; fields to a dataset with a dataspace of DIM0xDIM1, then
;;; closes the file.  Next, it reopens the file, reads back
;;; the data, and outputs it to the screen.


;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_bit.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_t_bit.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)


(defun pos (cols i j)
  "2D array position"
  (+ (* i cols) j))


(cffi:with-foreign-objects
    ((dims 'hsize-t 2)
     (wdata :unsigned-char (* *DIM0* *DIM1*)))

  ;; Initialize data.  We will manually pack 4 2-bit integers into
  ;; each unsigned char data element.

  (flet ((ABCD (i j)
	   (logior
	    (logior
	     (logior
	      (logior 0 (logand (- (* i j) j) #x03)) ; field "A"
	      (ash (logand i #x03) 2))               ; field "B" 
	     (ash (logand j #x03) 4))                ; field "C"
	    (ash (logand (+ i j) #x03) 6))))         ; field "D"
  
    (dotimes (i *DIM0*)
      (dotimes (j *DIM1*)
	(setf (cffi:mem-aref wdata :unsigned-char (pos *DIM1* i j))
	      (ABCD i j)))))
  
  ;; Create a new file using the default properties.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
		;; Create the dataset and write the bitfield data to it.
		(dset (h5dcreate2 file *DATASET* +H5T-STD-B8BE+ space
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	   (h5dwrite dset +H5T-NATIVE-B8+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     wdata)
	   ;; Close and release resources.
	   (h5ex:close-handles (list dset space)))
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
		(space (h5dget-space dset)))

	   ;; Get dataspace and allocate memory for read buffer.
	   (h5sget-simple-extent-dims space dims +NULL+)

	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0))
		 (dims[1] (cffi:mem-aref dims 'hsize-t 1)))
	     (cffi:with-foreign-object (rdata :unsigned-char
					      (* dims[0] dims[1]))
	       ;; Read the data.
	       (h5dread dset +H5T-NATIVE-B8+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
			rdata)

	       ;; Output the data to the screen.
	       (flet ((ABCD (x)
			(values (logand x #x03)
				(logand (ash x -2) #x03)
				(logand (ash x -4) #x03)
				(logand (ash x -6) #x03))))
		 (format t "~a:~%" *DATASET*)
		 (dotimes (i *DIM0*)
		   (format t " [")
		   (dotimes (j *DIM1*)
		     (multiple-value-bind (A B C D)
			 (ABCD (cffi:mem-aref rdata :unsigned-char
						  (pos *DIM1* i j)))
		       (format t " {~a, ~a, ~a, ~a}" A B C D)))
		   (format t " ]~%")))))

	   ;; Close and release resources.
	   (h5ex:close-handles (list space dset)))
      (h5ex:close-handles (list file fapl)))))
