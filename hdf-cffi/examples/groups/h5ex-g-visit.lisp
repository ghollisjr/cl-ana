;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; This example shows how to recursively traverse a file
;;; using H5Ovisit and H5Lvisit.  The program prints all of
;;; the objects in the file specified in FILE, then prints all
;;; of the links in that file.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5G/h5ex_g_visit.c

(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_g_visit.h5" *load-pathname*)))

;;; I'm not sure how to invoke a CFFI callback as a LISP function.
;;; This is a workaround...

(defun print-info-et-name (info name)
  (format t "/")
  (let ((type (cffi:foreign-slot-value info '(:struct h5o-info-t) 'type)))
    (if (equal name ".")
        (format t "  (Group)~%")
        (cond
          ((eql type :H5O-TYPE-GROUP)
           (format t "~a  (Group)~%" name))
          ((eql type :H5O-TYPE-DATASET)
           (format t "~a  (Dataset)~%" name))
          ((eql type :H5O-TYPE-NAMED-DATATYPE)
           (format t "~a  (Datatype)~%" name))
          (t (format t "~a  (Unknown)~%" name))))))

;;; the callback function for H5Ovisit

(cffi:defcallback op-func herr-t
  ((loc-id        hid-t)
   (name          :string)
   (info          (:pointer (:struct h5o-info-t)))
   (operator-data :pointer))
  (declare (ignore loc-id operator-data))
  (print-info-et-name info name)
  0)

;;; the callback function for H5Lvisit

(cffi:defcallback op-func-l herr-t
  ((loc-id        hid-t)
   (name          :string)
   (info          (:pointer (:struct h5l-info-t)))
   (operator-data :pointer))
  (declare (ignore info operator-data))
  (cffi:with-foreign-object (infobuf '(:struct h5o-info-t) 1)
    (h5oget-info-by-name loc-id name infobuf +H5P-DEFAULT+)
    (print-info-et-name infobuf name))
  0)

;;; Showtime

(let*
    ((fapl (h5pcreate +H5P-FILE-ACCESS+))
     (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
               (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       (progn
         (format t "Objects in the file:~%")
         (h5ovisit file :H5-INDEX-NAME :H5-ITER-NATIVE
                   (cffi:callback op-func) +NULL+)
         (format t "~%Links in the file:~%")
         (h5lvisit file :H5-INDEX-NAME :H5-ITER-NATIVE
                   (cffi:callback op-func-l) +NULL+)))
  (h5ex:close-handles (list file fapl)))
