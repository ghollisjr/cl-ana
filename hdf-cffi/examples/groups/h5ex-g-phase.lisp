;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.


;;; This example shows how to set the conditions for
;;; conversion between compact and dense (indexed) groups.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5G/h5ex_g_phase.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_g_phase.h5" *load-pathname*)))
(defparameter *MAX-GROUPS* 7)
(defparameter *MAX-COMPACT* 5)
(defparameter *MIN-DENSE* 3)


(defun print-groups-storage-type (nlinks type)
  (format t "~a Group~a: Storage type is " nlinks
	  (if (eql nlinks 1) " " "s"))
  (cond ((eql type :H5G-STORAGE-TYPE-COMPACT) ; New compact format
	 (format t "~a~%" "H5G_STORAGE_TYPE_COMPACT"))
	((eql type :H5G-STORAGE-TYPE-DENSE) ; New dense (indexed) format
	 (format t "~a~%" "H5G_STORAGE_TYPE_DENSE"))
	((eql type :H5G-STORAGE-TYPE-SYMBOL-TABLE) ; Original format
	 (format t "~a~%" "H5G_STORAGE_TYPE_SYMBOL_TABLE"))
	(t (format t "~a~%" "H5G_STORAGE_TYPE_UNKNOWN"))))


(cffi:with-foreign-objects ((ginfo '(:struct h5g-info-t) 1))

  ;; Set file access property list to allow the latest file format.
  ;; This will allow the library to create new format groups.

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (progn
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5pset-libver-bounds fapl :H5F-LIBVER-LATEST
				       :H5F-LIBVER-LATEST)
		 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))

    (unwind-protect
	 ;; Create group access property list and set the phase change
	 ;; conditions.  In this example we lowered the conversion
	 ;; threshold to simplify the output, though this may not be
	 ;; optimal.

	 (let* ((gcpl (h5pcreate +H5P-GROUP-CREATE+))
		(group (prog2 (h5pset-link-phase-change
			       gcpl *MAX-COMPACT* *MIN-DENSE*)
			   (h5gcreate2 file "G0" +H5P-DEFAULT+ gcpl
				       +H5P-DEFAULT+))))

	   ;; Add subgroups to "group" one at a time, print the storage
	   ;; type for "group" after each subgroup is created.
	   (dotimes (i *MAX-GROUPS*)

	     ;; Define the subgroup name and create the subgroup.
	     (cffi:with-foreign-string (name (format nil "G~a" (1+ i)))
	       (h5gclose (h5gcreate2 group name +H5P-DEFAULT+
				     +H5P-DEFAULT+ +H5P-DEFAULT+)))

	     ;; Obtain the group info and print the group storage type
	     (h5gget-info group ginfo)

	     (cffi:with-foreign-slots ((nlinks storage-type)
				       ginfo (:struct h5g-info-t))
	       (print-groups-storage-type nlinks storage-type)))

	   (format t "~%")

	   ;; Delete subgroups one at a time, print the storage type for
	   ;; "group" after each subgroup is deleted.
	   (dotimes (i *MAX-GROUPS*)

	     ;; Define the subgroup name and delete the subgroup.
	     (cffi:with-foreign-string (name (format nil "G~a"
						     (- *MAX-GROUPS* i)))
	       (h5ldelete group name +H5P-DEFAULT+))

	     ;; Obtain the group info and print the group storage type
	     (h5gget-info group ginfo)

	     (cffi:with-foreign-slots ((nlinks storage-type)
				       ginfo (:struct h5g-info-t))
	       (print-groups-storage-type nlinks storage-type)))

	   (h5ex:close-handles (list group gcpl)))
      (h5ex:close-handles (list file fapl)))))
