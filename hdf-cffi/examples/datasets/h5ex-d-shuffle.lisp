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
;;; using the shuffle filter with gzip compression.  The
;;; program first checks if the shuffle and gzip filters are
;;; available, then if they are it writes integers to a
;;; dataset using shuffle+gzip, then closes the file.  Next,
;;; it reopens the file, reads back the data, and outputs the
;;; types of filters and the maximum value in the dataset to
;;; the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_shuffle.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_shuffle.h5" *load-pathname*)))
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

  ;; Check if gzip compression is available and can be used for both
  ;; compression and decompression.  Normally we do not perform error
  ;; checking in these examples for the sake of clarity, but in this
  ;; case we will make an exception because this filter is an
  ;; optional part of the hdf5 library.

  (if (< (h5zfilter-avail +H5Z-FILTER-DEFLATE+) 1)
      (error "gzip filter not available."))

  (h5zget-filter-info +H5Z-FILTER-DEFLATE+ filter-info)
  (if (or (eql 0 (logand (cffi:mem-ref filter-info :unsigned-int)
			 +H5Z-FILTER-CONFIG-ENCODE-ENABLED+))
	  (eql 0 (logand (cffi:mem-ref filter-info :unsigned-int)
			 +H5Z-FILTER-CONFIG-DECODE-ENABLED+)))
      (error "gzip filter not available for encoding and decoding."))

  ;; Similarly, check for availability of the shuffle filter.
  (if (< (h5zfilter-avail +H5Z-FILTER-SHUFFLE+) 1)
      (error "Shuffle filter not available."))

  (h5zget-filter-info +H5Z-FILTER-SHUFFLE+ filter-info)
  (if (or (eql 0 (logand (cffi:mem-ref filter-info :unsigned-int)
			 +H5Z-FILTER-CONFIG-ENCODE-ENABLED+))
	  (eql 0 (logand (cffi:mem-ref filter-info :unsigned-int)
			 +H5Z-FILTER-CONFIG-DECODE-ENABLED+)))
      (error "Shuffle filter not available for encoding and decoding."))
  
  ;; Initialize data.
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) (- (* i j) j))))

  ;; Create a new file using the default properties.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
		(dcpl (h5pcreate +H5P-DATASET-CREATE+))
		;; Create the dataset.
		(dset (progn
                        ;; Create the dataset creation property list, add the
                        ;; shuffle filter and the gzip compression filter and
                        ;; set the chunk size. The order in which the filters
                        ;; are added here is significant - we will see much
                        ;; greater results when the shuffle is applied first.
                        ;; The order in which the filters are added to the
                        ;; property list is the order in which they
                        ;; will be invoked when writing data.
                        (h5pset-shuffle dcpl)
                        (h5pset-deflate dcpl 9)
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
		(nfilters (h5pget-nfilters dcpl)))

	   ;; Retrieve the number of filters, and retrieve and print the
	   ;; type of each.
	   (dotimes (i nfilters)
	     (setf (cffi:mem-ref nelmts 'size-t) 0)
	     (let ((filter-type (h5pget-filter2 dcpl i flags nelmts +NULL+
						0 +NULL+ filter-info)))
	       (format t "Filter ~a: Type is: " i)
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
		      (format t "H5Z_FILTER_SCALEOFFSET~%")))))

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
