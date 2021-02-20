;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to create "compact-or-indexed"
;;; format groups, new to 1.8.  This example also illustrates
;;; the space savings of compact groups by creating 2 files
;;; which are identical except for the group format, and
;;; displaying the file size of each.  Both files have one
;;; empty group in the root group.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5G/h5ex_g_compact.c





(in-package :hdf5)

(defparameter *FILE1* (namestring (merge-pathnames "h5ex_g_compact1.h5" *load-pathname*)))
(defparameter *FILE2* (namestring (merge-pathnames "h5ex_g_compact2.h5" *load-pathname*)))
(defparameter *GROUP* "G1")


(defun print-storage-type (type)
  (cond
    ((eql type :H5G-STORAGE-TYPE-SYMBOL-TABLE)
     (format t "~a~%" "H5G_STORAGE_TYPE_SYMBOL_TABLE"))
    ((eql type :H5G-STORAGE-TYPE-COMPACT)
     (format t "~a~%" "H5G_STORAGE_TYPE_COMPACT"))
    ((eql type :H5G-STORAGE-TYPE-DENSE)
     (format t "~a~%" "H5G_STORAGE_TYPE_DENSE"))
    (t (format t "~a~%" "H5G_STORAGE_TYPE_UNKNOWN"))))


(cffi:with-foreign-objects ((ginfo '(:struct h5g-info-t) 1)
			    (size 'hsize-t 1))
  
  ;; Create file 1.  This file will use original format groups.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE1* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 ;; Obtain the group info and print the group storage type.
	 (let ((group (h5gcreate2 file *GROUP* +H5P-DEFAULT+
				  +H5P-DEFAULT+ +H5P-DEFAULT+)))
	   (h5gget-info group ginfo)
	   (format t "Group storage type for ~a is: " *FILE1*)
	   (cffi:with-foreign-slots ((storage-type)
				     ginfo (:struct h5g-info-t))
	     (print-storage-type storage-type))
	   (h5gclose group))
      ;; Close and re-open file. Needed to get the correct file size.
      (h5ex:close-handles (list file fapl)))

    (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	   (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		     (h5fopen *FILE1* +H5F-ACC-RDONLY+ fapl))))
      (unwind-protect
	   (h5fget-filesize file size)
	(format t "File size for ~a is: ~a bytes~%~%" *FILE1*
		(cffi:mem-aref size 'hsize-t 0))
	(h5ex:close-handles (list file fapl))))

    ;; Set file access property list to allow the latest file format.
    ;; This will allow the library to create new compact format groups.

    ;; Set file access property list to allow the latest file format.
    ;; This will allow the library to create new compact format groups.
    ;;
    ;; Create file 2 using the new file access property list.

    (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	   (file (progn
		   (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5pset-libver-bounds fapl :H5F-LIBVER-LATEST
					 :H5F-LIBVER-LATEST)
		   (h5fcreate *FILE2* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
      (unwind-protect
	   (let ((group (h5gcreate2 file *GROUP* +H5P-DEFAULT+ +H5P-DEFAULT+
				    +H5P-DEFAULT+)))
	     ;; Obtain the group info and print the group storage type.
	     (h5gget-info group ginfo)
	     (format t "Group storage type for ~a is: " *FILE2*)
	     (cffi:with-foreign-slots ((storage-type)
				       ginfo (:struct h5g-info-t))
	       (print-storage-type storage-type))
	     (h5gclose group))
	;; Close and re-open file. Needed to get the correct file size.
	(h5ex:close-handles (list file fapl))))

    (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	   (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		     (h5fopen *FILE2* +H5F-ACC-RDONLY+ fapl))))
      (unwind-protect
	   (progn
	     (h5fget-filesize file size)
	     (format t "File size for ~a is: ~a bytes~%~%" *FILE2*
		     (cffi:mem-aref size 'hsize-t 0)))
	(h5ex:close-handles (list file fapl))))))
