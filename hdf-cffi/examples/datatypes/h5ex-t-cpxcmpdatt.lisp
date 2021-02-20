;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write a complex
;;; compound datatype to an attribute.  The program first
;;; writes complex compound structures to an attribute with a
;;; dataspace of DIM0, then closes the file.  Next, it reopens
;;; the file, reads back selected fields in the structure, and
;;; outputs them to the screen.

;;; Unlike the other datatype examples, in this example we
;;; save to the file using native datatypes to simplify the
;;; type definitions here.  To save using standard types you
;;; must manually calculate the sizes and offsets of compound
;;; types as shown in h5ex_t_cmpd.c, and convert enumerated
;;; values as shown in h5ex_t_enum.c.

;;; The datatype defined here consists of a compound
;;; containing a variable-length list of compound types, as
;;; well as a variable-length string, enumeration, double
;;; array, object reference and region reference.  The nested
;;; compound type contains an int, variable-length string and
;;; two doubles.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_cpxcmpdatt.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_cpxcmpdatt.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *ATTRIBUTE* "A1")
(defparameter *DIM0*    2)
(defparameter *LENA*    4)
(defparameter *LENB*    1)


(cffi:defcstruct sensor-t
  (serial-no   :int)
  (location    :string)
  (temperature :double)
  (pressure    :double))

(defun create-sensorstype()
  (flet ((create-sensortype ()
	   (let ((result (h5tcreate :H5T-COMPOUND
				    (cffi:foreign-type-size
				     '(:struct sensor-t))))
		 (strtype (h5ex:create-c-string-type)))
	     (h5tinsert result "Serial number"
			(cffi:foreign-slot-offset '(:struct sensor-t)
						  'serial-no)
			+H5T-NATIVE-INT+)
	     (h5tinsert result "Location"
			(cffi:foreign-slot-offset '(:struct sensor-t) 'location)
			strtype)
	     (h5tinsert result "Temperature (F)"
			(cffi:foreign-slot-offset '(:struct sensor-t)
						  'temperature)
			+H5T-NATIVE-DOUBLE+)
	     (h5tinsert result "Pressure (inHg)"
			(cffi:foreign-slot-offset '(:struct sensor-t) 'pressure)
			+H5T-NATIVE-DOUBLE+)
             (h5tclose strtype)
	     result)))
    (let* ((sensortype (create-sensortype))
	   (result (h5tvlen-create sensortype)))
      (h5tclose sensortype)
      result)))

(cffi:defcenum color-t
  :RED
  :GREEN
  :BLUE)

(defun create-colortype ()
  (cffi:with-foreign-object (val 'color-t)
    (let ((result (h5tenum-create +H5T-NATIVE-INT+)))
      (setf (cffi:mem-ref val 'color-t) :RED)
      (h5tenum-insert result "Red" val)
      (setf (cffi:mem-ref val 'color-t) :GREEN)
      (h5tenum-insert result "Green" val)
      (setf (cffi:mem-ref val 'color-t) :BLUE)
      (h5tenum-insert result "Blue" val)
      result)))


(cffi:defcstruct vehicle-t
  (sensors        (:struct hvl-t))
  (name           :string)
  (color          color-t)
  (location       :double :count 3)
  (group          hobj-ref-t)
  (surveyed-areas (:struct hdset-reg-ref-t)))

;;; Create the main compound datatype.

(defun create-vehicletype ()
  (let ((result (h5tcreate :H5T-COMPOUND
			   (cffi:foreign-type-size '(:struct vehicle-t))))
	(sensorstype (create-sensorstype))
	(strtype (h5ex:create-c-string-type))
	(colortype (create-colortype))
	(loctype (cffi:with-foreign-object (adims 'hsize-t 3)
		   (setf (cffi:mem-aref adims 'hsize-t 0) 3)
		   (h5tarray-create2 +H5T-NATIVE-DOUBLE+ 1 adims))))
    (h5tinsert result "Sensors"
	       (cffi:foreign-slot-offset '(:struct vehicle-t) 'sensors)
	       sensorstype)
    (h5tinsert result "Name"
	       (cffi:foreign-slot-offset '(:struct vehicle-t) 'name)
	       strtype)
    (h5tinsert result "Color"
	       (cffi:foreign-slot-offset '(:struct vehicle-t) 'color)
	       colortype)
    (h5tinsert result "Location"
	       (cffi:foreign-slot-offset '(:struct vehicle-t) 'location)
	       loctype)
    (h5tinsert result "Group"
	       (cffi:foreign-slot-offset '(:struct vehicle-t) 'group)
	       +H5T-STD-REF-OBJ+)
    (h5tinsert result "Surveyed areas"
	       (cffi:foreign-slot-offset '(:struct vehicle-t) 'surveyed-areas)
	       +H5T-STD-REF-DSETREG+)
    (h5ex:close-handles (list loctype colortype strtype sensorstype))
    result))

(defun create-rsensorstype ()
  (flet ((create-rsensortype ()
	   (let ((result (h5tcreate :H5T-COMPOUND
				    (cffi:foreign-type-size '(:pointer :char))))
		 (strtype (h5ex:create-c-string-type)))
	     (h5tinsert result "Location" 0 strtype)
	     (h5tclose strtype)
	     result)))
    (let* ((rsensortype (create-rsensortype))
	   (result (h5tvlen-create rsensortype)))
      (h5tclose rsensortype)
      result)))


(cffi:defcstruct rvehicle-t
  (sensors (:struct hvl-t))
  (name    :string))

;;; Create the nested compound datatype for reading.  Even though it
;;; has only one field, it must still be defined as a compound type
;;; so the library can match the correct field in the file type.
;;; This matching is done by name.  However, we do not need to
;;; define a structure for the read buffer as we can simply treat it
;;; as a char *.

(defun create-rvehicletype ()
  (let ((result (h5tcreate :H5T-COMPOUND
			   (cffi:foreign-type-size '(:struct rvehicle-t))))
	(strtype (h5ex:create-c-string-type))
	(rsensorstype (create-rsensorstype)))
    (h5tinsert result "Sensors"
	       (cffi:foreign-slot-offset '(:struct rvehicle-t) 'sensors)
	       rsensorstype)
    (h5tinsert result "Name"
	       (cffi:foreign-slot-offset '(:struct rvehicle-t) 'name)
	       strtype)
    (h5ex:close-handles (list rsensorstype strtype))
    result))


(cffi:with-foreign-objects
    ((adims2 'hsize-t 2)
     (start  'hsize-t 2)
     (count  'hsize-t 2)
     (coords 'hsize-t (* 3 2))
     (wdata  '(:struct vehicle-t) 2)
     (wdata2 :double (* 32 32)))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl)))
	 (ptrA (cffi:foreign-alloc '(:struct sensor-t) :count *LENA*))
	 (ptrB (cffi:foreign-alloc '(:struct sensor-t) :count *LENB*)))
    
    (unwind-protect
	 (progn
	   ;; create a dataset to use for region references
	   (dotimes (i 32)
	     (dotimes (j 32)
	       (setf (cffi:mem-aref wdata2 :double (+ (* i 32) j))
		     (+ 70.0d0
			(* 0.1d0 (- i 16.0d0))
			(* 0.1d0 (- j 16.0d0))))))
	   (let* ((shape (prog2 (setf (cffi:mem-aref adims2 'hsize-t 0) 32
				      (cffi:mem-aref adims2 'hsize-t 1) 32)
			     (h5screate-simple 2 adims2 +NULL+)))
		  (dset (h5dcreate2 file "Ambient_Temperature"
				    +H5T-NATIVE-DOUBLE+ shape
				    +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	     (h5dwrite dset +H5T-NATIVE-DOUBLE+ +H5S-ALL+ +H5S-ALL+
		       +H5P-DEFAULT+ wdata2)
	     (h5ex:close-handles (list dset shape)))
	   ;; create groups to use for object references
	   (h5ex:close-handles (list (h5gcreate2 file "Land_Vehicles"
                                                 +H5P-DEFAULT+ +H5P-DEFAULT+
                                                 +H5P-DEFAULT+)
                                     (h5gcreate2 file "Air_Vehicles"
                                                 +H5P-DEFAULT+ +H5P-DEFAULT+
                                                 +H5P-DEFAULT+)))
	   (let ((sensors-ptr
		  (cffi:foreign-slot-pointer
		   (cffi:mem-aptr wdata '(:struct vehicle-t) 0)
		   '(:struct vehicle-t) 'sensors))
		 (ptr[0] (cffi:mem-aptr ptrA '(:struct sensor-t) 0))
		 (ptr[1] (cffi:mem-aptr ptrA '(:struct sensor-t) 1))
		 (ptr[2] (cffi:mem-aptr ptrA '(:struct sensor-t) 2))
		 (ptr[3] (cffi:mem-aptr ptrA '(:struct sensor-t) 3)))
	     
	     ;; Initialize variable-length compound in the first data element.
	     (cffi:with-foreign-slots ((len p) sensors-ptr (:struct hvl-t))
	       (setf len *LENA* p ptrA))
	     (cffi:with-foreign-slots ((serial-no location temperature pressure)
				       ptr[0] (:struct sensor-t))
	       (setf serial-no 1153 location "Exterior (static)"
		     temperature 53.23d0 pressure 24.57d0))
	     (cffi:with-foreign-slots ((serial-no location temperature pressure)
				       ptr[1] (:struct sensor-t))
	       (setf serial-no 1184 location "Intake"
		     temperature 55.12d0 pressure 22.95d0))
	     (cffi:with-foreign-slots ((serial-no location temperature pressure)
				       ptr[2] (:struct sensor-t))
	       (setf serial-no 1027 location "Intake manifold"
		     temperature 103.55d0 pressure 31.23d0))
	     (cffi:with-foreign-slots ((serial-no location temperature pressure)
				       ptr[3] (:struct sensor-t))
	       (setf serial-no 1313 location "Exhaust manifold"
		     temperature 1252.89d0 pressure 84.11d0)))
	   
	   ;; Initialize other fields in the first data element.
	   (let ((wdata[0] (cffi:mem-aptr wdata '(:struct vehicle-t) 0))
		 (shape (prog2 (setf (cffi:mem-aref adims2 'hsize-t 0) 32
				     (cffi:mem-aref adims2 'hsize-t 1) 32)
			    (h5screate-simple 2 adims2 +NULL+))))
	     (cffi:with-foreign-slots ((name color location)
				       wdata[0] (:struct vehicle-t))
	       (setf name "Airplane"
		     color :GREEN
		     (cffi:mem-aref location :double 0) -103234.21d0
	             (cffi:mem-aref location :double 1) 422638.78d0
		     (cffi:mem-aref location :double 2) 5996.43d0))
	     (h5rcreate (cffi:foreign-slot-pointer
			 wdata[0] '(:struct vehicle-t) 'group)
			file "Air_Vehicles" :H5R-OBJECT -1)	     
	     (h5sselect-elements shape :H5S-SELECT-SET 3 coords)
	     (h5rcreate (cffi:foreign-slot-pointer
			 wdata[0] '(:struct vehicle-t) 'surveyed-areas)
			file "Ambient_Temperature" :H5R-DATASET-REGION shape)
	     (h5sclose shape))
	   ;; Initialize variable-length compound in the second data element.
	   (let ((sensors-ptr
		  (cffi:foreign-slot-pointer
		   (cffi:mem-aptr wdata '(:struct vehicle-t) 1)
		   '(:struct vehicle-t) 'sensors))
		 (ptr[0] (cffi:mem-aptr ptrB '(:struct sensor-t) 0)))

	     (cffi:with-foreign-slots ((len p) sensors-ptr (:struct hvl-t))
	       (setf len *LENB* p ptrB))
	     (cffi:with-foreign-slots ((serial-no location temperature pressure)
				       ptr[0] (:struct sensor-t))
	       (setf serial-no 3244 location "Roof"
		     temperature 83.82d0 pressure 29.92d0)))
	   ;; Initialize other fields in the second data element.
	   (let ((wdata[1] (cffi:mem-aptr wdata '(:struct vehicle-t) 1))
		 (shape (prog2 (setf (cffi:mem-aref adims2 'hsize-t 0) 32
				     (cffi:mem-aref adims2 'hsize-t 1) 32)
			    (h5screate-simple 2 adims2 +NULL+))))
	     (cffi:with-foreign-slots ((name color location)
				       wdata[1] (:struct vehicle-t))
	       (setf name "Automobile"
		     color :RED
		     (cffi:mem-aref location :double 0) 326734.36d0
	             (cffi:mem-aref location :double 1) 221568.23d0
		     (cffi:mem-aref location :double 2) 432.36d0))
	     (h5rcreate (cffi:foreign-slot-pointer
			 wdata[1] '(:struct vehicle-t) 'group)
			file "Land_Vehicles" :H5R-OBJECT -1)	     
	     (setf (cffi:mem-aref start 'hsize-t 0) 8
		   (cffi:mem-aref start 'hsize-t 1) 26
		   (cffi:mem-aref count 'hsize-t 0) 4
		   (cffi:mem-aref count 'hsize-t 1) 3)
	     (h5sselect-hyperslab shape :H5S-SELECT-SET start +NULL+
				  count +NULL+)
	     (h5rcreate (cffi:foreign-slot-pointer wdata[1] '(:struct vehicle-t)
						   'surveyed-areas)
			file "Ambient_Temperature" :H5R-DATASET-REGION shape)
	     (h5sclose shape))
	   (let* ((vehicletype (create-vehicletype))
                  ;; Create dataset with a null dataspace. to serve as the
                  ;; parent for the attribute.
                  (dspace (h5ex:create-null-dataspace))
                  (dset (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ dspace
                                    +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
		  (aspace (h5ex:create-simple-dataspace `(,*DIM0*)))
                    ;; Create the attribute and write the compound data to it.
		  (attr (h5acreate2 dset *ATTRIBUTE* vehicletype aspace
				    +H5P-DEFAULT+ +H5P-DEFAULT+)))
             (h5awrite attr vehicletype wdata)
             (h5ex:close-handles (list attr aspace dset dspace vehicletype))))
      (cffi:foreign-free ptrB)
      (cffi:foreign-free ptrA)
      (h5ex:close-handles (list file fapl)))))

(cffi:with-foreign-objects
    ((ndims 'hsize-t 1)
     (dims 'hsize-t 1))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
                (attr (h5aopen dset *ATTRIBUTE* +H5P-DEFAULT+))
		(rvehicletype (create-rvehicletype))
		(space (h5aget-space attr)))
	   (setf (cffi:mem-ref ndims 'hsize-t 0)
		 (h5sget-simple-extent-dims space dims +NULL+))
	   ;;	Allocate memory for read buffer.
	   (let ((rdata (cffi:foreign-alloc '(:struct rvehicle-t)
					    :count (cffi:mem-aref dims
								  'hsize-t 0))))
	     ;; Read the data.
             (h5aread attr rvehicletype rdata)
	     ;; Output the data to the screen.
	     (dotimes (i *DIM0*)
	       (let* ((rdata-ptr
		       (cffi:mem-aptr rdata '(:struct rvehicle-t) i))
		      (rdata-sensors-ptr
		       (cffi:foreign-slot-pointer
			rdata-ptr '(:struct rvehicle-t) 'sensors)))
		 (format t "~a[~d]:~%" *ATTRIBUTE* i)
		 (format t "   Vehicle name :~%      ~a~%"
			 (cffi:foreign-slot-value
			  rdata-ptr '(:struct rvehicle-t) 'name))
		 (format t "   Sensor locations :~%")
		 (dotimes (j (cffi:foreign-slot-value
			      rdata-sensors-ptr '(:struct hvl-t) 'len))
		   (format t "      ~a~%"
			   (cffi:mem-ref
			    (cffi:mem-aptr
			     (cffi:foreign-slot-value
			      rdata-sensors-ptr '(:struct hvl-t) 'p)
			     :pointer j)
			    :string)))))
	     ;; Close and release resources. H5Dvlen_reclaim will
	     ;; automatically traverse the structure and free any vlen
	     ;; data (including strings).
	     (h5dvlen-reclaim rvehicletype space +H5P-DEFAULT+ rdata)
	     (cffi:foreign-free rdata))

	   (h5ex:close-handles (list space rvehicletype attr dset)))
      (h5ex:close-handles (list file fapl)))))
