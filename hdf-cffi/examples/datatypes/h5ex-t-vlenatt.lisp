;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write variable-length
;;; datatypes to an attribute.  The program first writes two
;;; variable-length integer arrays to the attribute then
;;; closes the file.  Next, it reopens the file, reads back
;;; the data, and outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_vlenatt.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_vlen.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *ATTRIBUTE* "A1")
(defparameter *LEN0* 3)
(defparameter *LEN1* 12)


(defun create-wdata ()
  (let* ((wdata (cffi::foreign-alloc '(:struct hvl-t) :count 2))
         (wdata[0] (cffi:mem-aptr wdata '(:struct hvl-t) 0))
         (wdata[1] (cffi:mem-aptr wdata '(:struct hvl-t) 1)))
    ;; Initialize variable-length data.  wdata[0] is a countdown of
    ;; length LEN0, wdata[1] is a Fibonacci sequence of length LEN1.
    (cffi:with-foreign-slots ((len p) wdata[0] (:struct hvl-t))
      (setf len *LEN0*
            p (cffi:foreign-alloc :int :count *LEN0*
                                  :initial-contents (loop for i from 0
                                                       to (1- *LEN0*)
                                                       collect (- *LEN0* i)))))
    (cffi:with-foreign-slots ((len p) wdata[1] (:struct hvl-t))
      (labels ((fib (n)
               (cond ((< n 0) nil) 
                     ((or (= n 0) (= n 1)) 1)
                     (t (+ (fib (1- n)) (fib (- n 2)))))))
        (setf len *LEN1*
              p (cffi:foreign-alloc :int :count *LEN1*
                                    :initial-contents (loop for i from 0
                                                         to (1- *LEN1*)
                                                         collect (fib i))))))
    wdata))


;; Create a new file using the default properties.
(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((wdata (create-wdata))
              (filetype (h5tvlen-create +H5T-STD-I32LE+))
              (memtype (h5tvlen-create +H5T-NATIVE-INT+))
              (dspace (h5ex:create-null-dataspace))
              (aspace (h5ex:create-simple-dataspace '(2)))
              ;; Create dataset with a null dataspace.
              (dset (h5dcreate2 file *DATASET* +H5T-STD-I32LE+  dspace
                                +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
              ;; Create the attribute and write the variable-length data to it
              (attr (h5acreate2 dset *ATTRIBUTE* filetype aspace
                                +H5P-DEFAULT+ +H5P-DEFAULT+)))
         (h5awrite attr memtype wdata)

         ;; Close and release resources. Note the use of H5Dvlen_reclaim
         ;; removes the need to manually free() the previously malloc'ed
         ;; data.
         (h5dvlen-reclaim memtype aspace +H5P-DEFAULT+ wdata)
         (h5ex:close-handles (list attr dset dspace aspace memtype filetype))
         (cffi:foreign-free wdata))
    (h5ex:close-handles (list file fapl))))

;; Now we begin the read section of this example.  Here we assume
;; the attribute has the same name and rank, but can have any size.
;; Therefore we must allocate a new array to read in data dynamically.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))

  ;; Get dataspace and allocate memory for array of vlen structures.
  ;; This does not actually allocate memory for the vlen data, that
  ;; will be done by the library.
  (unwind-protect
       (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
              (attr (h5aopen dset *ATTRIBUTE* +H5P-DEFAULT+))
              (space (h5aget-space attr)))
         (cffi:with-foreign-object (dims 'hsize-t 1)
           (h5sget-simple-extent-dims space dims +NULL+)
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0))
                 (memtype (h5tvlen-create +H5T-NATIVE-INT+)))
	     (cffi:with-foreign-object (rdata '(:struct hvl-t) dims[0])
               ;; Read the data.
	       (h5aread attr memtype rdata)

	       ;; Output the variable-length data to the screen.
               (dotimes (i dims[0])
                 (format t "~a[~d]:~%  {" *ATTRIBUTE* i)
                 (let ((rdata[i] (cffi:mem-aptr rdata '(:struct hvl-t) i)))
                   (cffi:with-foreign-slots ((len p) rdata[i] (:struct hvl-t))
                     (dotimes (j len)
                       (format t " ~d" (cffi:mem-aref p :int j))
                       (when (< (1+ j) len) (format t ",")))
                     (format t " }~%"))))

               ;; Close and release resources.  Note we must still free the
               ;; top-level pointer "rdata", as H5Dvlen_reclaim only frees the
               ;; actual variable-length data, and not the structures
               ;; themselves.
               (h5dvlen-reclaim memtype space +H5P-DEFAULT+ rdata))
             (h5tclose memtype)))
         (h5ex:close-handles (list space attr dset)))
    (h5ex:close-handles (list file fapl))))
