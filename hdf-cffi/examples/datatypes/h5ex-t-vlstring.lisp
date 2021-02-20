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
(defparameter *DIM0* 4)


(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((wdata (cffi:foreign-alloc
                      :string :initial-contents
                      '("Parting" "is such" "sweet" "sorrow")))
              (ftype (h5ex:create-f-string-type))
              (mtype (h5ex:create-c-string-type))
              (shape (h5ex:create-simple-dataspace `(,*DIM0*)))
              (dset (h5dcreate2 file *DATASET* ftype shape +H5P-DEFAULT+
                                +H5P-DEFAULT+ +H5P-DEFAULT+)))
         (h5dwrite dset mtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ wdata)
         ;; Close and release resources.
         (h5ex:close-handles (list dset shape mtype ftype))
         (cffi:foreign-free wdata))
    (h5ex:close-handles (list file fapl))))

;; Now we begin the read section of this example.  Here we assume
;; the dataset has the same name and rank, but can have any size.
;; Therefore we must allocate a new array to read dynamically.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
              (shape (h5dget-space dset)) 
              (mtype (h5ex:create-c-string-type)))
         (cffi:with-foreign-object (dims 'hsize-t 1)
           (h5sget-simple-extent-dims shape dims +NULL+)
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0)))
             (cffi:with-foreign-object (rdata '(:pointer :char) dims[0])
               ;; Read the data.
               (h5dread dset mtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rdata)
               ;; Output the data to the screen.
               (dotimes (i dims[0])
                 (format t "~a[~d]: ~a~%" *DATASET* i
                         (cffi:foreign-string-to-lisp
                          (cffi:mem-aref rdata '(:pointer :char) i))))
               ;; Close and release resources.  Note that H5Dvlen_reclaim works
               ;; for variable-length strings as well as variable-length arrays.
               ;; H5Tvlen_reclaim only frees the data these point to.
               (h5dvlen-reclaim mtype shape +H5P-DEFAULT+ rdata))))
         (h5ex:close-handles (list mtype shape dset)))
    (h5ex:close-handles (list file fapl))))
