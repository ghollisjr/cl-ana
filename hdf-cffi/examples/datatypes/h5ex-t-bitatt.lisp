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
;;; datatypes to an attribute.  The program first writes bit
;;; fields to an attribute with a dataspace of DIM0xDIM1, then
;;; closes the file.  Next, it reopens the file, reads back
;;; the data, and outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_bitatt.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_t_bitatt.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *ATTRIBUTE* "A1")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)


(defun pos (cols i j)
  "2D array position"
  (+ (* i cols) j))


(cffi:with-foreign-objects
    ((dims 'hsize-t 2)
     (wdata :unsigned-char (* *DIM0* *DIM1*)))

  (setf (cffi:mem-aref dims 'hsize-t 0) *DIM0*
	(cffi:mem-aref dims 'hsize-t 1) *DIM1*)

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
	 ;; Create dataset with null dataspace.
	 (let* ((dspace (h5ex:create-null-dataspace))
		(dset (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ dspace
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
                (aspace (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
                ;; Create the attribute and write the bitfield data to it.
                (attr (h5acreate2 dset *ATTRIBUTE* +H5T-STD-B8BE+ aspace
                                  +H5P-DEFAULT+ +H5P-DEFAULT+)))
           (h5awrite attr +H5T-NATIVE-B8+ wdata)
	   ;; Close and release resources.
           (h5ex:close-handles (list attr aspace dset dspace)))
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
		(attr (h5aopen dset *ATTRIBUTE* +H5P-DEFAULT+))
		(space (h5aget-space attr)))

	   ;; Get dataspace and allocate memory for read buffer.
	   (h5sget-simple-extent-dims space dims +NULL+)

	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0))
		 (dims[1] (cffi:mem-aref dims 'hsize-t 1)))
	     (cffi:with-foreign-object (rdata :unsigned-char
					      (* dims[0] dims[1]))
	       ;; Read the data.
	       (h5aread attr +H5T-NATIVE-B8+ rdata)

	       ;; Output the data to the screen.
	       (flet ((ABCD (x)
			(values (logand x #x03)
				(logand (ash x -2) #x03)
				(logand (ash x -4) #x03)
				(logand (ash x -6) #x03))))
		 (format t "~a:~%" *ATTRIBUTE*)
		 (dotimes (i *DIM0*)
		   (format t " [")
		   (dotimes (j *DIM1*)
		     (multiple-value-bind (A B C D)
			 (ABCD (cffi:mem-aref rdata :unsigned-char
						  (pos *DIM1* i j)))
		       (format t " {~a, ~a, ~a, ~a}" A B C D)))
		   (format t " ]~%")))))

	   ;; Close and release resources.
	   (h5ex:close-handles (list space attr dset)))
      (h5ex:close-handles (list file fapl)))))
