;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write data to a compact
;;; dataset.  The program first writes integers to a compact
;;; dataset with dataspace dimensions of DIM0xDIM1, then
;;; closes the file.  Next, it reopens the file, reads back
;;; the data, and outputs it to the screen. 

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_compact.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_compact.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)


(cffi:with-foreign-objects ((wdata :int (* *DIM0* *DIM1*))
			    (rdata :int (* *DIM0* *DIM1*)))
  ;; initialize data
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) (- (* i j) j))))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
		(dcpl (h5pcreate +H5P-DATASET-CREATE+))
		;; Create the dataset using the dataset creation property
		;; list.
		(dset (progn
                        ;; Create the dataset creation property list, set the
                        ;; layout to compact.
                        (h5pset-layout dcpl :H5D-COMPACT)
                        ;; Create the dataset. We will use all default
                        ;; properties for this example.
                        (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ space
                                    +H5P-DEFAULT+ dcpl +H5P-DEFAULT+))))
	   ;; Write the data to the dataset.
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     wdata)

	   ;; Close and release resources.
	   (h5ex:close-handles (list dset dcpl space)))
      (h5ex:close-handles (list file fapl))))

  ;; Now we begin the read section of this example.

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
		(dcpl (h5dget-create-plist dset))
		(layout (h5pget-layout dcpl)))
	   (format t "Storage layout for ~a is: " *DATASET*)
	   ;; Retrieve the dataset creation property list, and print the
	   ;; storage layout.
	   (cond ((eql layout :H5D-COMPACT) (format t "H5D_COMPACT~%"))
		 ((eql layout :H5D-CONTIGUOUS) (format t "H5D_CONTIGUOUS~%"))
		 ((eql layout :H5D-CHUNKED) (format t "H5D_CHUNKED~%")))

	   ;;Read the data using the default properties.
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     rdata)

	   ;; Output the data to the screen.
	   (format t "~a:~%" *DATASET*)
	   (dotimes (i *DIM0*)
	     (format t " [")
	     (dotimes (j *DIM1*)
	       (format t " ~3d" (cffi:mem-aref rdata :int
                                               (h5ex:pos2D *DIM1* i j))))
	     (format t "]~%"))

	   ;; Close and release resources.
	   (h5ex:close-handles (list dcpl dset)))
      (h5ex:close-handles (list file fapl)))))
