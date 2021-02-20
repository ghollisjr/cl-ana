;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write region references
;;; to a dataset.  The program first creates a dataset
;;; containing characters and writes references to region of
;;; the dataset to a new dataset with a dataspace of DIM0,
;;; then closes the file.  Next, it reopens the file,
;;; dereferences the references, and outputs the referenced
;;; regions to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_regref.c





(in-package :hdf5)

(defparameter *FILE*     (namestring (merge-pathnames "h5ex_t_regref.h5" *load-pathname*)))
(defparameter *DATASET*  "DS1")
(defparameter *DATASET2* "DS2")
(defparameter *DIM0*   2)
(defparameter *DS2DIM0* 3)
(defparameter *DS2DIM1* 16)

;; Create a new file using the default properties.
(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       ;; Create a dataset with character data.
       (let* ((space (h5ex:create-simple-dataspace (list *DS2DIM0*
                                                         *DS2DIM1*)))
              (dset2 (h5dcreate2 file *DATASET2* +H5T-STD-I8LE+ space
                                 +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
              (data (concatenate 'string
                                 "The quick brown\0"
                                 "fox jumps over \0"
                                 "the 5 lazy dogs\0"))
              (wdata2 (cffi:foreign-string-alloc data
                                                 :null-terminated-p nil)))
         (h5dwrite dset2 +H5T-NATIVE-CHAR+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                   wdata2)

         (cffi:with-foreign-object (wdata '(:struct hdset-reg-ref-t) 2)

           ;; Create reference to a list of elements in dset2.
           (let ((coords (cffi:foreign-alloc 'hsize-t :count (* 4 2)
                                             :initial-contents
                                             '(0 1 2 11 1 0 2 4))))
             (h5sselect-elements space :H5S-SELECT-SET 4 coords)
             (cffi:foreign-free coords))

           (h5rcreate (cffi:mem-aptr wdata '(:struct hdset-reg-ref-t) 0)
                      file *DATASET2* :H5R-DATASET-REGION space)

           ;; Create reference to a hyperslab in dset2.
           (cffi:with-foreign-objects ((start 'hsize-t 2)
                                       (stride 'hsize-t 2)
                                       (count 'hsize-t 2)
                                       (block 'hsize-t 2))
             (setf (cffi:mem-aref start 'hsize-t 0) 0
                   (cffi:mem-aref start 'hsize-t 1) 0
                   (cffi:mem-aref stride 'hsize-t 0) 2
                   (cffi:mem-aref stride 'hsize-t 1) 11
                   (cffi:mem-aref count 'hsize-t 0) 2
                   (cffi:mem-aref count 'hsize-t 1) 2
                   (cffi:mem-aref block 'hsize-t 0) 1
                   (cffi:mem-aref block 'hsize-t 1) 3)
             (h5sselect-hyperslab space :H5S-SELECT-SET start stride count
                                  block))

           (h5rcreate (cffi:mem-aptr wdata '(:struct hdset-reg-ref-t) 1)
                      file *DATASET2* :H5R-DATASET-REGION space)

           ;; Create the dataset and write the region references to it.
           (let* ((space (h5ex:create-simple-dataspace `(,*DIM0*)))
                  (dset (h5dcreate2 file *DATASET* +H5T-STD-REF-DSETREG+
                                    space +H5P-DEFAULT+ +H5P-DEFAULT+
                                    +H5P-DEFAULT+)))
             (h5dwrite dset +H5T-STD-REF-DSETREG+ +H5S-ALL+ +H5S-ALL+
                       +H5P-DEFAULT+ wdata)
             (h5ex:close-handles (list dset space)))

           ;; Close and release resources.
           (cffi:foreign-free wdata2)
           (h5ex:close-handles (list dset2 space))))

    (h5ex:close-handles (list file fapl))))

;;; Now we begin the read section of this example.  Here we assume
;;; the dataset has the same name and rank, but can have any size.
;;; Therefore we must allocate a new array to read in data dynamically.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       ;; Get dataspace and allocate memory for read buffer.
       (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
              (space (h5dget-space dset)))
         (cffi:with-foreign-object (dims 'hsize-t 1)
           (h5sget-simple-extent-dims space dims +NULL+)
           (let* ((dims[0] (cffi:mem-aref dims 'hsize-t 0))
                  (rdata (cffi:foreign-alloc '(:struct hdset-reg-ref-t)
                                             :count dims[0])))
             ;; Read the data.
             (h5dread dset +H5T-STD-REF-DSETREG+ +H5S-ALL+ +H5S-ALL+
                      +H5P-DEFAULT+ rdata)

             ;; Output the data to the screen.
             (dotimes (i dims[0])
               (format t "~a[~d]:~%  ->" *DATASET* i)

               ;; Open the referenced object, retrieve its region as a
               ;; dataspace selection.
               (let* ((rdata[i] (cffi:mem-aptr rdata
                                               '(:struct hdset-reg-ref-t) i))
                      (dset2 (if (= +H5-VERS-MINOR+ 8)
                                 (h5rdereference dset :H5R-DATASET-REGION
                                                rdata[i])
                                 (h5rdereference dset +H5P-DEFAULT+
                                                 :H5R-DATASET-REGION rdata[i])))
                      (space (h5rget-region dset :H5R-DATASET-REGION rdata[i]))
                      ;; Get the length of the object's name, allocate space,
                      ;; then retrieve the name.
                      (size (1+ (h5iget-name dset2 +NULL+ 0)))
                      (name (cffi:foreign-alloc :char :count size))
                      ;; Allocate space for the read buffer. We will only
                      ;; allocate enough space for the selection, plus a null
                      ;; terminator.  The read buffer will be 1-dimensional.
                      (npoints (h5sget-select-npoints space))
                      (rdata2 (cffi:foreign-alloc :char :count (1+ npoints))))
                 (h5iget-name dset2 name size)

                 ;; Read the dataset region, and add a null terminator so we
                 ;; can print it as a string.
                 (cffi:with-foreign-object (dims 'hsize-t 1)
                   (setf (cffi:mem-aref dims 'hsize-t 0) npoints)
                   (let ((memspace (h5screate-simple 1 dims +NULL+)))
                     (h5dread dset2 +H5T-NATIVE-CHAR+ memspace space
                              +H5P-DEFAULT+ rdata2)
                     (setf (cffi:mem-aref rdata2 :char npoints) 0)
                     ;; Print the name and region data, close and release
                     ;; resources.
                     (format t "~a: ~a~%"
                             (cffi:foreign-string-to-lisp name)
                             (cffi:foreign-string-to-lisp rdata2))
                     (h5sclose memspace)))

                 ;; Close and release resources.
                 (cffi:foreign-free rdata2)
                 (cffi:foreign-free name)
                 (h5ex:close-handles (list space dset2))))

             (cffi:foreign-free rdata))
           (h5ex:close-handles (list space dset))))

    (h5ex:close-handles (list file fapl))))
