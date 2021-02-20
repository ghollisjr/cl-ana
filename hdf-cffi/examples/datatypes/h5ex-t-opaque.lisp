;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write opaque datatypes
;;; to a dataset.  The program first writes opaque data to a
;;; dataset with a dataspace of DIM0, then closes the file.
;;; Next, it reopens the file, reads back the data, and
;;; outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_opaque.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_opaque.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *LEN* 7)

;; Initialize data.

(defun create-wdata (dim)
  (let* ((data (format nil "~{~a~}"
                       (mapcar #'(lambda (x) (format nil "OPAQUE~d" x))
                               (loop for i from 0 to (1- dim) collect i))))
         (wdata (cffi:foreign-string-alloc data :null-terminated-p nil)))
    wdata))


;; Create opaque datatype and set the tag to something appropriate.
;; For this example we will write and view the data as a character array.

(defun create-opaque-type (length tag)
  (let ((dtype (h5tcreate :H5T-OPAQUE length))
        (tag-ptr (cffi:foreign-string-alloc tag)))
    (h5tset-tag dtype tag-ptr)
    (cffi:foreign-string-free tag-ptr)
    dtype))


;; Create a new file using the default properties.
(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((wdata (create-wdata *DIM0*))
              (dtype (create-opaque-type *LEN* "Character array"))
              (space (h5ex:create-simple-dataspace `(,*DIM0*)))
              ;; Create the dataset and write the opaque data to it.
              (dset (h5dcreate2 file *DATASET* dtype space
                                +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
         (h5dwrite dset dtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ wdata)
         (h5ex:close-handles (list dset space dtype))
         (cffi:foreign-free wdata))
    (h5ex:close-handles (list file fapl))))

;; Now we begin the read section of this example.  Here we assume
;; the dataset has the same name and rank, but can have any size.
;; Therefore we must allocate a new array to read in data dynamically.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       ;; Get datatype and properties for the datatype.  Note that H5Tget_tag
       ;; allocates space for the string in tag, so we must remember to free()
       ;; it later.
       (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
              (space (h5dget-space dset))
              (dtype (h5dget-type dset))
              (len (h5tget-size dtype))
              (tag (h5tget-tag dtype)))

         ;; Get dataspace and allocate memory for read buffer.
         (cffi:with-foreign-object (dims 'hsize-t 1)
           (h5sget-simple-extent-dims space dims +NULL+)
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0)))
	     (cffi:with-foreign-object (rdata :char (* dims[0] len))
	       ;; Read the data.
	       (h5dread dset dtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rdata)
	       ;; Output the data to the screen.
               (format t "Datatype tag for ~a is: \"~a\"~%"
                       *DATASET* (cffi:foreign-string-to-lisp tag))
               (dotimes (i dims[0])
                 (format t "~a[~d]: " *DATASET* i)
                 (format t "~a"
                         (cffi:foreign-string-to-lisp rdata :offset (* i len)
                                                      :max-chars len))
                 (format t "~%")))))

         ;; Close and release resources.
         (cffi:foreign-free tag)
         (h5ex:close-handles (list dtype space dset)))
    (h5ex:close-handles (list file fapl))))
