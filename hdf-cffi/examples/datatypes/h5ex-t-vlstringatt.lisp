;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; This example shows how to read and write variable-length
;;; string datatypes to a dataset.  The program first writes
;;; variable-length strings to a dataset with a dataspace of
;;; DIM0, then closes the file.  Next, it reopens the file,
;;; reads back the data, and outputs it to the screen.

;;; See h5ex_t_vlstring.c at http://www.hdfgroup.org/HDF5/examples/api18-c.html





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_t_vlstring.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *ATTRIBUTE* "A1")
(defparameter *DIM0* 4)


(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((wdata (cffi:foreign-alloc
                      :string :initial-contents
                      '("Parting" "is such" "sweet" "sorrow")))
              (filetype (h5ex:create-f-string-type))
              (memtype (h5ex:create-c-string-type))
              (dshape (h5ex:create-null-dataspace))
              (ashape (h5ex:create-simple-dataspace `(,*DIM0*)))
              ;; Create dataset with a null dataspace.
              (dset (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ dshape
                                +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
              ;; Create the attribute and write the variable-length string data
              ;; to it.
              (attr (h5acreate2 dset *ATTRIBUTE* filetype ashape +H5P-DEFAULT+
                                +H5P-DEFAULT+)))
         (h5awrite attr memtype wdata)
         ;; Close and release resources.
         (h5ex:close-handles (list attr dset ashape dshape memtype filetype))
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
              (shape (h5aget-space attr))
              (memtype (h5ex:create-c-string-type)))
         (cffi:with-foreign-object (dims 'hsize-t 1)
           (h5sget-simple-extent-dims shape dims +NULL+)
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0)))
             (cffi:with-foreign-object (rdata '(:pointer :char) dims[0])
               ;; Read the data.
               (h5aread attr memtype rdata)
               ;; Output the data to the screen.
               (dotimes (i dims[0])
                 (format t "~a[~s]: ~a~%" *ATTRIBUTE* i
                         (cffi:foreign-string-to-lisp
                          (cffi:mem-aref rdata '(:pointer :char) i))))
               ;; Close and release resources.  Note that H5Dvlen_reclaim works
               ;; for variable-length strings as well as variable-length arrays.
               ;; H5Tvlen_reclaim only frees the data these point to.
               (h5dvlen-reclaim memtype shape +H5P-DEFAULT+ rdata))))
         (h5ex:close-handles (list memtype shape attr dset))
    (h5ex:close-handles (list file fapl)))))
