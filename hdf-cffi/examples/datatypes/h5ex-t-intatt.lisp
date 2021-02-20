;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write integer datatypes
;;; to an attribute.  The program first writes integers to an
;;; attribute with a dataspace of DIM0xDIM1, then closes the
;;; file.  Next, it reopens the file, reads back the data, and
;;; outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_intatt.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_intatt.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *ATTRIBUTE* "A1")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)


(defun pos (cols i j)
  "2D array position"
  (+ (* i cols) j))


(defun create-wdata (rows cols)
  (let ((wdata (cffi::foreign-alloc :int :count (* rows cols))))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (cffi:mem-aref wdata :int (pos cols i j))
              (- (* i j) j))))
    wdata))


;; Create a new file using the default properties.
(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((wdata (create-wdata *DIM0* *DIM1*))
              (dspace (h5ex:create-null-dataspace))
              (aspace (h5ex:create-simple-dataspace (list *DIM0* *DIM1*)))
              ;; Create dataset with a null dataspace.
              (dset (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ dspace
                                +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
              (attr (h5acreate2 dset *ATTRIBUTE* +H5T-STD-I64BE+ aspace
                                +H5P-DEFAULT+ +H5P-DEFAULT+)))
         (h5awrite attr +H5T-NATIVE-INT+ wdata)
         ;; Close and release resources.
         (h5ex:close-handles (list attr dset aspace dspace))
         (cffi:foreign-free wdata))
    (h5ex:close-handles (list file fapl))))

;; Now we begin the read section of this example.  Here we assume
;; the attribute has the same name and rank, but can have any size.
;; Therefore we must allocate a new array to read in data dynamically.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
              (attr (h5aopen dset *ATTRIBUTE* +H5P-DEFAULT+))
              (space (h5aget-space attr)))

         (cffi:with-foreign-object (dims 'hsize-t 2)
           (h5sget-simple-extent-dims space dims +NULL+)
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0))
		 (dims[1] (cffi:mem-aref dims 'hsize-t 1)))
	     ;; Allocate space for integer data.
	     (cffi:with-foreign-object (rdata :int (* dims[0] dims[1]))
	       ;; Read the data.
	       (h5aread attr +H5T-NATIVE-INT+ rdata)
	       ;; Output the data to the screen.
               (format t "~a:~%" *ATTRIBUTE*)
               (dotimes (i dims[0])
                 (format t " [")
                 (dotimes (j dims[1])
                   (format t " ~3d"
                           (cffi:mem-aref rdata :int (pos dims[1] i j))))
                 (format t "]~%")))))

         ;; Close and release resources.
         (h5ex:close-handles (list space attr dset)))
    (h5ex:close-handles (list file fapl))))
