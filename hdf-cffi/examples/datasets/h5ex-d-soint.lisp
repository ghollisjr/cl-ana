;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write data to a dataset
;;; using the Scale-Offset filter.  The program first checks
;;; if the Scale-Offset filter is available, then if it is it
;;; writes integers to a dataset using Scale-Offset, then
;;; closes the file Next, it reopens the file, reads back the
;;; data, and outputs the type of filter and the maximum value
;;; in the dataset to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_soint.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_soint.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 32)
(defparameter *DIM1* 64)
(defparameter *CHUNK0* 4)
(defparameter *CHUNK1* 8)


(cffi:with-foreign-objects ((chunk 'hsize-t 2)
			    (filter-info :unsigned-int 1)
			    (flags :unsigned-int 1)
			    (nelmts 'size-t 1)
			    (wdata :int (* *DIM0* *DIM1*))
			    (rdata :int (* *DIM0* *DIM1*)))

  (setf (cffi:mem-aref nelmts 'size-t 0) 0)

  ;; Check if Scale-Offset compression is available and can be used for both
  ;; compression and decompression.  Normally we do not perform error
  ;; checking in these examples for the sake of clarity, but in this
  ;; case we will make an exception because this filter is an
  ;; optional part of the hdf5 library.

  (if (< (h5zfilter-avail +H5Z-FILTER-SCALEOFFSET+) 1)
      (error "Scale-Offset filter not available."))

  (h5zget-filter-info +H5Z-FILTER-SCALEOFFSET+ filter-info)
  (if (or (eql 0 (logand (cffi:mem-ref filter-info :unsigned-int)
			 +H5Z-FILTER-CONFIG-ENCODE-ENABLED+))
	  (eql 0 (logand (cffi:mem-ref filter-info :unsigned-int)
			 +H5Z-FILTER-CONFIG-DECODE-ENABLED+)))
      (error "Scale-Offset filter not available for encoding and decoding."))

  ;; Initialize data.
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) (- (* i j) j))))

  ;; Create a new file using the default properties.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2
		   (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
		(dcpl (h5pcreate +H5P-DATASET-CREATE+))
		;; Create the dataset using the dataset creation property
		;; list.
		(dset (progn
                        ;; Create the dataset creation property list, add the
                        ;; Scale-Offset filter and set the chunk size.
                        (h5pset-scaleoffset dcpl :H5Z-SO-INT
                                            +H5Z-SO-INT-MINBITS-DEFAULT+)
                        (setf (cffi:mem-aref chunk 'hsize-t 0) *CHUNK0*
                              (cffi:mem-aref chunk 'hsize-t 1) *CHUNK1*)
                        (h5pset-chunk dcpl 2 chunk)
                        ;; Create the chunked dataset.
                        (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ space
                                    +H5P-DEFAULT+ dcpl +H5P-DEFAULT+))))

	   ;; Write the data to the dataset.
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ space +H5P-DEFAULT+ wdata)

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
		;; Retrieve and print the filter type. Here we only retrieve
		;; the first filter because we know that we only added one
		;; filter.
		(filter-type (h5pget-filter2 dcpl 0 flags nelmts +NULL+
					     0 +NULL+ filter-info)))
	   (format t "Filter type is: ")

	   (cond ((eql filter-type +H5Z-FILTER-DEFLATE+)
		  (format t "H5Z_FILTER_DEFLATE~%"))
		 ((eql filter-type +H5Z-FILTER-SHUFFLE+)
		  (format t "H5Z_FILTER_SHUFFLE~%"))
		 ((eql filter-type +H5Z-FILTER-FLETCHER32+)
		  (format t "H5Z_FILTER_FLETCHER32~%"))
		 ((eql filter-type +H5Z-FILTER-SZIP+)
		  (format t "H5Z_FILTER_SZIP~%"))
		 ((eql filter-type +H5Z-FILTER-NBIT+)
		  (format t "H5Z_FILTER_NBIT~%"))
		 ((eql filter-type +H5Z-FILTER-SCALEOFFSET+)
		  (format t "H5Z_FILTER_SCALEOFFSET~%")))

	   ;; Read the data using the default properties.
	   (h5dread dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                    rdata)

	   ;; Find the maximum value in the dataset, to verify that it was
	   ;; read correctly.
           (format t "Maximum value in ~a is: ~a~%" *DATASET*
                   (reduce #'max
                           (mapcar #'(lambda (i) (cffi:mem-aref rdata :int i))
                                   (loop for i from 0 to (1- (* *DIM0* *DIM1*))
                                      collect i))))

	   ;; Close and release resources.
	   (h5ex:close-handles (list dcpl dset)))
      (h5ex:close-handles (list file fapl)))))
