;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write enumerated
;;; datatypes to a dataset.  The program first writes
;;; enumerated values to a dataset with a dataspace of
;;; DIM0xDIM1, then closes the file.  Next, it reopens the
;;; file, reads back the data, and outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_enum.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_enum.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)
(defparameter *F-BASET* +H5T-STD-I16BE+)
(defparameter *M-BASET* +H5T-NATIVE-INT+)
(defparameter *NAME-BUF-SIZE* 16)

(cffi:defcenum phase-t
    :SOLID
    :LIQUID
    :GAS
    :PLASMA)

(defun create-enumtypes ()
  (let ((filetype (h5tenum-create *F-BASET*))
	(memtype (h5tenum-create *M-BASET*))
	(names (cffi:foreign-alloc :string :initial-contents
				   '("SOLID" "LIQUID" "GAS" "PLASMA"))))
    (cffi:with-foreign-object (val 'phase-t)
      (dotimes (i (1+ (cffi:foreign-enum-value 'phase-t :PLASMA)))
	(setf (cffi:mem-ref val 'phase-t) i)
	;; Insert enumerated value for memtype.
	(h5tenum-insert memtype
			(cffi::mem-aref names :string i) val)
	;; Insert enumerated value for filetype.  We must first convert
	;; the numerical value val to the base type of the destination.
	(h5tconvert *M-BASET* *F-BASET* 1 val +NULL+ +H5P-DEFAULT+)
	(h5tenum-insert filetype
			(cffi::mem-aref names :string i) val)))
    ;; return TWO datatype handles
    (values filetype memtype)))


(defun pos (cols i j)
  "2D array position"
  (+ (* i cols) j))


(cffi:with-foreign-object (wdata 'phase-t (* *DIM0* *DIM1*))

  ;; Initialize data.
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata 'phase-t (pos *DIM1* i j))
	    (rem (- (* (1+ i) j) j)
		 (1+ (cffi:foreign-enum-value 'phase-t :PLASMA))))))
  
  ;; Create a new file using the default properties.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (multiple-value-bind (filetype memtype)
	     (create-enumtypes)
	   (let* ((space (h5ex:create-simple-dataspace (list *DIM0* *DIM1*)))
		  (dset (h5dcreate2 file *DATASET* filetype space
				    +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	     (h5dwrite dset memtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ wdata)
	     ;; Close and release resources.
             (h5ex:close-handles (list dset space memtype filetype))))
      (h5ex:close-handles (list file fapl)))))

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
         (cffi:with-foreign-object (dims 'hsize-t 2)
	   (h5sget-simple-extent-dims space dims +NULL+)
	   ;; Allocate space for integer data.
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0))
		 (dims[1] (cffi:mem-aref dims 'hsize-t 1)))

	     (cffi:with-foreign-objects ((rdata 'phase-t (* dims[0] dims[1]))
                                         (name :char *NAME-BUF-SIZE*))
	       (multiple-value-bind (filetype memtype)
		   (create-enumtypes)
		 ;; Read the data.
		 (h5dread dset memtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rdata)
		 ;; Output the data to the screen.
		 (format t "~a:~%" *DATASET*)
		 (dotimes (i *DIM0*)
		   (format t " [")
		   (dotimes (j *DIM1*)
		     ;; Get the name of the enumeration member.
		     (h5tenum-nameof memtype
				     (cffi:mem-aptr rdata 'phase-t
						    (pos *DIM1* i j))
				     name *NAME-BUF-SIZE*)
		     (format t " ~6a" (cffi:foreign-string-to-lisp name)))
		   (format t "]~%"))
		 (h5ex:close-handles (list filetype memtype))))))
	   (h5ex:close-handles (list space dset)))
      (h5ex:close-handles (list file fapl))))
