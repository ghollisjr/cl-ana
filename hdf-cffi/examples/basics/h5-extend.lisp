;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example how to work with extendible datasets. The dataset
;;; must be chunked in order to be extendible.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_extend.c





(in-package :hdf5)

(defparameter *FILENAME* (namestring (merge-pathnames "extend.h5" *load-pathname*)))
(defparameter *DATASETNAME* "ExtendibleArray")
(defparameter *RANK* 2)

(cffi:with-foreign-objects
    ((dims 'hsize-t 2)
     (chunk-dims 'hsize-t 2)
     (data :int (* 3 3))
     (size 'hsize-t 2)
     (offset 'hsize-t 2)
     (dimsext 'hsize-t 2)
     (dataext :int (* 7 3))
     (chunk-dimsr 'hsize-t 2)
     (dimsr 'hsize-t 2)
     (rdata :int (* 10 3)))

  (setf (cffi:mem-aref dims 'hsize-t 0) 3
	(cffi:mem-aref dims 'hsize-t 1) 3)
  (setf (cffi:mem-aref chunk-dims 'hsize-t 0) 2
	(cffi:mem-aref chunk-dims 'hsize-t 1) 5)

  (dotimes (i 3)
    (dotimes (j 3)
      (setf (cffi:mem-aref data :int (h5ex:pos2D 3 i j)) 1)))

  (setf (cffi:mem-aref dimsext 'hsize-t 0) 7
	(cffi:mem-aref dimsext 'hsize-t 1) 3)

  (dotimes (i 7)
    (dotimes (j 3)
      (let ((pos (h5ex:pos2D 3 i j)))
	(cond ((= j 0) (setf (cffi:mem-aref dataext :int pos) 2))
	      ((= j 1) (setf (cffi:mem-aref dataext :int pos) 3))
	      ((= j 2) (setf (cffi:mem-aref dataext :int pos) 4))))))
  
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILENAME* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))

    (unwind-protect
	 (let* ((shape (h5ex:create-simple-dataspace '(3 3)
						     `(,+H5S-UNLIMITED+
						       ,+H5S-UNLIMITED+)))
		(dcpl (h5pcreate +H5P-DATASET-CREATE+))
		(dset (prog2 (h5pset-chunk dcpl *RANK* chunk-dims)
			  (h5dcreate2 file *DATASETNAME* +H5T-NATIVE-INT+ shape
				      +H5P-DEFAULT+ dcpl +H5P-DEFAULT+))))
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     data)

	   ;; extend the dataset
	   (setf (cffi:mem-aref size 'hsize-t 0)
		 (+ (cffi:mem-ref dims 'hsize-t 0)
		    (cffi:mem-ref dimsext 'hsize-t 0))
		 (cffi:mem-aref size 'hsize-t 1) 3)
	   (h5dset-extent dset size)

	   (let ((fshape (h5dget-space dset))
		 (mshape (h5ex:create-simple-dataspace '(7 3))))
	     (setf (cffi:mem-aref offset 'hsize-t 0) 3
		   (cffi:mem-aref offset 'hsize-t 1) 0)
	     (h5sselect-hyperslab fshape :H5S-SELECT-SET offset +NULL+ dimsext
				  +NULL+)
	     (h5dwrite dset +H5T-NATIVE-INT+ mshape fshape +H5P-DEFAULT+
		       dataext)
	     (h5ex:close-handles (list mshape fshape)))
	   (h5ex:close-handles (list dset dcpl shape)))
      (h5ex:close-handles (list file fapl))))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILENAME* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let* ((dset (h5dopen2 file *DATASETNAME* +H5P-DEFAULT+))
		(fshape (h5dget-space dset))
		(rank (h5sget-simple-extent-ndims fshape))
		(plist (h5dget-create-plist dset)))

	   (if (eql :H5D-CHUNKED (h5pget-layout plist))
	       (h5pget-chunk plist rank chunk-dimsr))

	   (h5sget-simple-extent-dims fshape dimsr +NULL+)

	   (let ((mshape (h5screate-simple rank dimsr +NULL+))
		 (dimsr[0] (cffi:mem-aref dimsr 'hsize-t 0))
		 (dimsr[1] (cffi:mem-aref dimsr 'hsize-t 1)))
	     (h5dread dset +H5T-NATIVE-INT+ mshape fshape +H5P-DEFAULT+ rdata)
	     (format t "~%")
	     (format t "Dataset: ~%")
	     (dotimes (i dimsr[0])
	       (dotimes (j dimsr[1])
		 (format t "~d " (cffi:mem-aref rdata :int
						(h5ex:pos2D dimsr[1] i j))))
	       (format t "~%"))
	     (h5sclose mshape))
	   (h5ex:close-handles (list plist fshape dset)))
      (h5ex:close-handles (list file fapl)))))
