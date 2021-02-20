;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example illustrates how to create a dataset that is a 4 x 6 array.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_crtdat.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "dset.h5" *load-pathname*)))

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((shape (h5ex:create-simple-dataspace '(4 6)))
	      (dset (h5dcreate2 file "/dset" +H5T-STD-I32BE+ shape
				+H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	 (h5ex:close-handles (list dset shape)))
    (h5ex:close-handles (list file fapl))))
