;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to set the fill value for a
;;; dataset.  The program first sets the fill value to
;;; FILLVAL, creates a dataset with dimensions of DIM0xDIM1,
;;; reads from the uninitialized dataset, and outputs the
;;; contents to the screen.  Next, it writes integers to the
;;; dataset, reads the data back, and outputs it to the
;;; screen.  Finally it extends the dataset, reads from it,
;;; and outputs the result to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_fillval.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_fillval.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *DIM1* 7)
(defparameter *EDIM0* 6)
(defparameter *EDIM1* 10)
(defparameter *CHUNK0* 4)
(defparameter *CHUNK1* 4)
(defparameter *FILLVAL* 99)


(cffi:with-foreign-objects ((extdims 'hsize-t 2)
			    (chunk 'hsize-t 2)
			    (wdata :int (* *DIM0* *DIM1*))
			    (rdata :int (* *DIM0* *DIM1*))
			    (rdata2 :int (* *EDIM0* *EDIM1*))
			    (fillval :int 1))
  ;; initialize the data to be written
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) (- (* i j) j))))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
         (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)
                                                     `(,+H5S-UNLIMITED+
                                                       ,+H5S-UNLIMITED+)))
                (dcpl (h5pcreate +H5P-DATASET-CREATE+))
                ;; Create the dataset using the dataset creation property
                ;; list.
                (dset (progn
                        (setf (cffi:mem-aref chunk 'hsize-t 0) *CHUNK0*
                              (cffi:mem-aref chunk 'hsize-t 1) *CHUNK1*)
                        ;; Set the chunk size
                        (h5pset-chunk dcpl 2 chunk)
                        (setf (cffi:mem-aref fillval :int 0) 99)
                        (h5pset-fill-value dcpl +H5T-NATIVE-INT+ fillval)
                        ;; Set the allocation time to "early". This way we
                        ;; can be sure that reading from the dataset immediately
                        ;; after creation will return the fill value.
                        (h5pset-alloc-time dcpl :H5D-ALLOC-TIME-EARLY)
                        (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ space
                                    +H5P-DEFAULT+ dcpl +H5P-DEFAULT+))))

           ;; Read values from the dataset, which has not been written to yet.
           (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                     rdata)
           ;; Output the data to the screen.
           (format t "Dataset before being written to:~%")
           (dotimes (i *DIM0*)
             (format t " [")
             (dotimes (j *DIM1*)
               (format t " ~3d" (cffi:mem-aref rdata :int
                                               (h5ex:pos2D *DIM1* i j))))
             (format t "]~%"))
           ;; Write the data to the dataset
           (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                     wdata)

           ;; Read the data back
           (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                     rdata)
           ;; Output the data to the screen.
           (format t "~%Dataset after being written:~%")
           (dotimes (i *DIM0*)
             (format t " [")
             (dotimes (j *DIM1*)
               (format t " ~3d" (cffi:mem-aref rdata :int
                                               (h5ex:pos2D *DIM1* i j))))
             (format t "]~%"))

           ;; Extend the dataset.
           (setf (cffi:mem-aref extdims 'hsize-t 0) *EDIM0*
                 (cffi:mem-aref extdims 'hsize-t 1) *EDIM1*)
           (h5dset-extent dset extdims)
           ;; Read from the extended dataset.
           (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                     rdata2)
           ;; Output the data to the screen.
           (format t "~%Dataset after extension:~%")
           (dotimes (i *EDIM0*)
             (format t " [")
             (dotimes (j *EDIM1*)
               (format t " ~3d" (cffi:mem-aref rdata2 :int
                                               (h5ex:pos2D *EDIM1* i j))))
             (format t "]~%"))

           ;; Close and release resources.
           (h5ex:close-handles (list dset dcpl space)))
      (h5ex:close-handles (list file fapl)))))
