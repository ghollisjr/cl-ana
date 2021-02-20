;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example illustrates how to create a dataset in a group.
;;; It depends on the HDF5 file created by h5-crtgrpar.lisp.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_crtgrpd.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "groups.h5" *load-pathname*)))

(cffi:with-foreign-objects
    ((dset1-data :int (* 3 3))
     (dset2-data :int (* 2 10)))

  ;; initialize the data of the first dataset
  (dotimes (i 3)
    (dotimes (j 3)
      (setf (cffi:mem-aref dset1-data :int (h5ex:pos2D 3 i j)) (1+ j))))

  ;; initialize the data of the second dataset
  (dotimes (i 2)
    (dotimes (j 10)
      (setf (cffi:mem-aref dset2-data :int (h5ex:pos2D 10 i j)) (1+ j))))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDWR+ fapl))))
    (unwind-protect
	 (progn
	   ;; create a first dataset in group '/MyGroup' 
	   (let* ((shape (h5ex:create-simple-dataspace '(3 3)))
		  (dset (h5dcreate2 file "/MyGroup/dset1" +H5T-STD-I32BE+ shape
				    +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	     ;; write to the first dataset
	     (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		       dset1-data)
	     (h5ex:close-handles (list dset shape)))

	   ;; create a second dataset in '/MyGroup/Group_A'
	   (let* ((grp (h5gopen2 file "/MyGroup/Group_A" +H5P-DEFAULT+))
		  (shape (h5ex:create-simple-dataspace '(2 10)))
		  (dset (h5dcreate2 grp "dset2" +H5T-STD-I32BE+ shape
				    +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	     ;; write to the second dataset
	     (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		       dset2-data)
	     (h5ex:close-handles (list dset shape grp))))
      (h5ex:close-handles (list file fapl)))))
