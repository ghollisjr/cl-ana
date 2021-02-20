;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to create, open, and close a group.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5G/h5ex_g_create.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_g_create.h5" *load-pathname*)))

;;; Create a new file using the default properties.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (progn
	 ;; Create a group named "G1" in the file.
	 (h5gclose (h5gcreate2 file "/G1" +H5P-DEFAULT+
			       +H5P-DEFAULT+ +H5P-DEFAULT+))
	 ;; Re-open the group, obtaining a new handle.
	 (let ((group (h5gopen2 file "/G1" +H5P-DEFAULT+)))
	   (h5gclose group)))
    (h5ex:close-handles (list file fapl))))
