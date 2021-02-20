;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write data to an
;;; external dataset.  The program first writes integers to an
;;; external dataset with dataspace dimensions of DIM0xDIM1,
;;; then closes the file.  Next, it reopens the file, reads
;;; back the data, and outputs the name of the external data
;;; file and the data to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5D/h5ex_d_extern.c






(in-package :hdf5)

(defvar *FILE* (namestring (merge-pathnames "h5ex_d_extern.h5" *load-pathname*)))
(defvar *EXTERNAL* (namestring (merge-pathnames "h5ex_d_extern.data" *load-pathname*)))
(defvar *DATASET* "DS1")
(defvar *DIM0* 4)
(defvar *DIM1* 7)
(defvar *NAME-BUF-SIZE* 32)


(cffi:with-foreign-objects ((name :char *NAME-BUF-SIZE*)
			    (wdata :int (* *DIM0* *DIM1*))
			    (rdata :int (* *DIM0* *DIM1*)))
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
		(dset (prog2
                          ;; Create the dataset creation property list, and
                          ;; set the external file.
                          (h5pset-external dcpl *EXTERNAL* 0 +H5F-UNLIMITED+)
                          ;; Create the external dataset
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
		(dcpl (h5dget-create-plist dset)))

	   ;; Retrieve and print the name of the external file.  Here we
	   ;; manually set the last field in name to null, in case the name of
	   ;; the file is longer than the buffer.
	   (h5pget-external dcpl 0 *NAME-BUF-SIZE* name +NULL+ +NULL+)
	   (setf (cffi:mem-aref name :char (1- *NAME-BUF-SIZE*)) 0)
	   (format t "~a is stored in file: ~a~%" *DATASET*
		   (cffi:foreign-string-to-lisp name))

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
