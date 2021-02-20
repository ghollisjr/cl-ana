;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example illustrates how to create and close a group.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_crtgrp.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "group.h5" *load-pathname*)))

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let ((g (h5gcreate2 file "/MyGroup"
			    +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	 (h5gclose g))
    (h5ex:close-handles (list file fapl))))
