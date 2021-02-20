;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example illustrates how to write and read data in an existing
;;; dataset. It depends on the HDF5 file created by h5-crtdat.lisp.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_rdwt.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "dset.h5" *load-pathname*)))

(cffi:with-foreign-object (dset-data :int (* 4 6))

  ;; initialize the data to be written
  (dotimes (i 4)
    (dotimes (j 6)
      (let ((pos (h5ex:pos2D 6 i j)))
	(setf (cffi:mem-aref dset-data :int pos) (1+ pos)))))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDWR+ fapl))))
    (unwind-protect
	 (let ((dset (h5dopen2 file "/dset" +H5P-DEFAULT+))) ; open the dataset
	   ;; write the dataset
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     dset-data)
	   ;; and read it back
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     dset-data)
	   (h5dclose dset))
      (h5ex:close-handles (list file fapl)))))
