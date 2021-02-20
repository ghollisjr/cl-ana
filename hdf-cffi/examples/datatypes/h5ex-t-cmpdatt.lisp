;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write compound
;;; datatypes to an attribute.  The program first writes
;;; compound structures to an attribute with a dataspace of
;;; DIM0, then closes the file.  Next, it reopens the file,
;;; reads back the data, and outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_cmpdatt.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_cmpdatt.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *ATTRIBUTE* "A1")
(defparameter *DIM0*    4)


(cffi:defcstruct sensor-t
  (serial-no   :int)
  (location    :string)
  (temperature :double)
  (pressure    :double))


(defun create-memtype ()
  (let ((strtype (h5ex:create-c-string-type))
	(result (h5tcreate :H5T-COMPOUND
			   (cffi:foreign-type-size '(:struct sensor-t)))))
    (h5tinsert result "Serial number"
	       (cffi:foreign-slot-offset '(:struct sensor-t) 'serial-no)
	       +H5T-NATIVE-INT+)
    (h5tinsert result "Location"
	       (cffi:foreign-slot-offset '(:struct sensor-t) 'location)
	       strtype)
    (h5tinsert result "Temperature (F)"
	       (cffi:foreign-slot-offset '(:struct sensor-t) 'temperature)
	       +H5T-NATIVE-DOUBLE+)
    (h5tinsert result "Pressure (inHg)"
	       (cffi:foreign-slot-offset '(:struct sensor-t) 'pressure)
	       +H5T-NATIVE-DOUBLE+)
    (h5tclose strtype)
    result))


(defun create-filetype ()

  ;; Create the compound datatype for the file. Because the
  ;; standard types we are using for the file may have different
  ;; sizes than the corresponding native types, we must manually
  ;; calculate the offset of each member.
	
  (let ((strtype (h5ex:create-c-string-type))
	(result (h5tcreate :H5T-COMPOUND
			   (+ 8 (cffi:foreign-type-size '(:struct hvl-t)) 8
			      8))))
    (h5tinsert result "Serial number" 0 +H5T-STD-I64BE+)
    (h5tinsert result "Location" 8 strtype)
    (h5tinsert result "Temperature (F)" (+ 8 (cffi:foreign-type-size
					      '(:struct hvl-t)))
	       +H5T-IEEE-F64BE+)
    (h5tinsert result "Pressure (inHg)" (+ 8 (cffi:foreign-type-size
					      '(:struct hvl-t))
					   8)
	       +H5T-IEEE-F64BE+)
    (h5tclose strtype)
    result))


(cffi:with-foreign-object (wdata '(:struct sensor-t) *DIM0*)
  
  ;; Initialize data.
  (let ((wdata[0] (cffi:mem-aptr wdata '(:struct sensor-t) 0))
	(wdata[1] (cffi:mem-aptr wdata '(:struct sensor-t) 1))
	(wdata[2] (cffi:mem-aptr wdata '(:struct sensor-t) 2))
	(wdata[3] (cffi:mem-aptr wdata '(:struct sensor-t) 3)))
    (cffi:with-foreign-slots ((serial-no location temperature pressure)
			      wdata[0] (:struct sensor-t))
      (setf serial-no 1153 location "Exterior (static)" temperature 53.23d0
	    pressure 24.57d0))
    (cffi:with-foreign-slots ((serial-no location temperature pressure)
			      wdata[1] (:struct sensor-t))
      (setf serial-no 1184 location "Intake" temperature 55.12d0
	    pressure 22.95d0))
    (cffi:with-foreign-slots ((serial-no location temperature pressure)
			      wdata[2] (:struct sensor-t))
      (setf serial-no 1027 location "Intake manifold" temperature 103.55d0
	    pressure 31.23d0))
    (cffi:with-foreign-slots ((serial-no location temperature pressure)
			      wdata[3] (:struct sensor-t))
      (setf serial-no 1313 location "Exhaust manifold" temperature 1252.89d0
	    pressure 84.11d0)))
  
  ;; Create a new file using the default properties.
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let* ((dspace (h5ex:create-null-dataspace))
		(dset (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ dspace
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
		(filetype (create-filetype))
                (aspace (h5ex:create-simple-dataspace `(,*DIM0*)))
		(attr (h5acreate2 dset *ATTRIBUTE* filetype aspace
                                  +H5P-DEFAULT+ +H5P-DEFAULT+))
		(memtype (create-memtype)))
	   (h5awrite attr memtype wdata)

	   ;; Close and release resources.
	   (h5ex:close-handles (list memtype attr aspace filetype dset dspace)))
      (h5ex:close-handles (list file fapl)))))

;; Now we begin the read section of this example. Here we assume
;; the attribute has the same name and rank, but can have any size.
;; Therefore we must allocate a new array to read in data dynamicaly

(cffi:with-foreign-object (dims 'hsize-t 1)
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
		(attr (h5aopen dset *ATTRIBUTE* +H5P-DEFAULT+))
		(space (h5aget-space attr))
		(memtype (create-memtype)))

	   ;; Get dataspace and allocate memory for read buffer.
	   (h5sget-simple-extent-dims space dims +NULL+)
	   (let* ((dims[0] (cffi:mem-aref dims 'hsize-t 0))
		  (rdata (cffi:foreign-alloc '(:struct sensor-t)
					     :count dims[0])))
	     (h5aread attr memtype rdata)

	     ;; Output the data to the screen.
	     (dotimes (i dims[0])
	       (format t "~a[~a]:~%" *ATTRIBUTE* i)
	       (cffi:with-foreign-slots ((serial-no location temperature
						    pressure)
					 (cffi:mem-aptr rdata
							'(:struct sensor-t) i)
					 (:struct sensor-t))
		 (format t "Serial number   : ~d~%" serial-no)
		 (format t "Location        : ~a~%" location)
		 (format t "Temperature (F) : ~6$~%" temperature)
		 (format t "Pressure (inHg) : ~6$~%~%" pressure)))

	     (h5dvlen-reclaim memtype space +H5P-DEFAULT+ rdata)
	     (cffi:foreign-free rdata))

	   (h5ex:close-handles (list memtype space attr dset)))
      (h5ex:close-handles (list file fapl)))))
