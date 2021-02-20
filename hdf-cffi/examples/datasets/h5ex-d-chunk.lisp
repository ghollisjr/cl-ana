;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to create a chunked dataset.  The
;;;  program first writes integers in a hyperslab selection to
;;; a chunked dataset with dataspace dimensions of DIM0xDIM1
;;; and chunk size of CHUNK0xCHUNK1, then closes the file.
;;; Next, it reopens the file, reads back the data, and
;;; outputs it to the screen.  Finally it reads the data again
;;; using a different hyperslab selection, and outputs
;;; the result to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_chunk.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_d_chunk.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 6)
(defparameter *DIM1* 8)
(defparameter *CHUNK0* 4)
(defparameter *CHUNK1* 4)


(cffi:with-foreign-objects ((chunk 'hsize-t 2)
			    (start 'hsize-t 2)
			    (stride 'hsize-t 2)
			    (count 'hsize-t 2)
			    (block 'hsize-t 2)
			    (wdata :int (* *DIM0* *DIM1*))
			    (rdata :int (* *DIM0* *DIM1*)))
  ;; Initialize data to "1", to make it easier to see the selections.
  (dotimes (i *DIM0*)
    (dotimes (j *DIM1*)
      (setf (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j)) 1)))

  ;; Print the data to the screen.
  (format t "Original Data:~%")
  (dotimes (i *DIM0*)
    (format t " [")
    (dotimes (j *DIM1*)
      (format t " ~3d" (cffi:mem-aref wdata :int (h5ex:pos2D *DIM1* i j))))
    (format t "]~%"))

  ;; Create a new file using the default properties.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((space (h5ex:create-simple-dataspace `(,*DIM0* ,*DIM1*)))
		(dcpl (h5pcreate +H5P-DATASET-CREATE+))
		;; Create the dataset using the dataset creation property
		;; list.
                (dset (progn
                        ;; Create the dataset creation property list, and
                        ;; set the chunk size.
                        (setf (cffi:mem-aref chunk 'hsize-t 0) *CHUNK0*
                              (cffi:mem-aref chunk 'hsize-t 1) *CHUNK1*)
                        (h5pset-chunk dcpl 2 chunk)
                        ;; Create the chunked dataset.
                        (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ space
                                    +H5P-DEFAULT+ dcpl +H5P-DEFAULT+))))

	   ;; Define and select the first part of the hyperslab selection.
	   (setf (cffi:mem-aref start 'hsize-t 0) 0
		 (cffi:mem-aref start 'hsize-t 1) 0
		 (cffi:mem-aref stride 'hsize-t 0) 3
		 (cffi:mem-aref stride 'hsize-t 1) 3
		 (cffi:mem-aref count 'hsize-t 0) 2
		 (cffi:mem-aref count 'hsize-t 1) 3
		 (cffi:mem-aref block 'hsize-t 0) 2
		 (cffi:mem-aref block 'hsize-t 1) 2)
	   (h5sselect-hyperslab space :H5S-SELECT-SET start stride count block)

	   ;; Define and select the second part of the hyperslab selection,
	   ;; which is subtracted from the first selection by the use of
	   ;; H5S_SELECT_NOTB
	   (setf (cffi:mem-aref block 'hsize-t 0) 1
		 (cffi:mem-aref block 'hsize-t 1) 1)
	   (h5sselect-hyperslab space :H5S-SELECT-NOTB start stride count block)

	   ;; Write the data to the dataset.
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ space +H5P-DEFAULT+
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
		(layout (h5pget-layout dcpl))
		(space (h5dget-space dset)))
	   (format t "~%Storage layout for ~a is: " *DATASET*)
	   ;; Retrieve the dataset creation property list, and print the
	   ;; storage layout.
	   (cond ((eql layout :H5D-COMPACT) (format t "H5D_COMPACT~%"))
		 ((eql layout :H5D-CONTIGUOUS) (format t "H5D_CONTIGUOUS~%"))
		 ((eql layout :H5D-CHUNKED) (format t "H5D_CHUNKED~%")))

	   ;;Read the data using the default properties.
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     rdata)

	   ;; Output the data to the screen.
	   (format t "~%Data as written to disk by hyberslabs:~%")
	   (dotimes (i *DIM0*)
	     (format t " [")
	     (dotimes (j *DIM1*)
	       (format t " ~3d" (cffi:mem-aref rdata :int
                                               (h5ex:pos2D *DIM1* i j))))
	     (format t "]~%"))

	   ;; Initialize the read array.
	   (dotimes (i *DIM0*)
	     (dotimes (j *DIM1*)
	       (setf (cffi:mem-aref rdata :int (h5ex:pos2D *DIM1* i j)) 0)))

	   ;; Define and select the hyperslab to use for reading.
	   (setf (cffi:mem-aref start 'hsize-t 0) 0
		 (cffi:mem-aref start 'hsize-t 1) 1
		 (cffi:mem-aref stride 'hsize-t 0) 4
		 (cffi:mem-aref stride 'hsize-t 1) 4
		 (cffi:mem-aref count 'hsize-t 0) 2
		 (cffi:mem-aref count 'hsize-t 1) 2
		 (cffi:mem-aref block 'hsize-t 0) 2
		 (cffi:mem-aref block 'hsize-t 1) 3)
	   (h5sselect-hyperslab space :H5S-SELECT-SET start stride count block)

	   ;; Read the data using the previously defined hyperslab.
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ space +H5P-DEFAULT+
		     rdata)

	   ;; Output the data to the screen.
	   (format t "~%Data as read from disk by hyperslab:~%")
	   (dotimes (i *DIM0*)
	     (format t " [")
	     (dotimes (j *DIM1*)
	       (format t " ~3d" (cffi:mem-aref rdata :int
                                               (h5ex:pos2D *DIM1* i j))))
	     (format t "]~%"))

	   ;; Close and release resources.
	   (h5ex:close-handles (list space dcpl dset)))
      (h5ex:close-handles (list file fapl)))))
