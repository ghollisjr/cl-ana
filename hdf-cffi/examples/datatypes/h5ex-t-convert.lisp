;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to convert between different
;;; datatypes in memory.  The program converts DIM0 elements
;;; of compound type sourcetype to desttype, then outputs the
;;; converted data to the screen.  A background buffer is used
;;; to fill in the elements of desttype that are not in
;;; sourcetype.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_convert.c







(in-package :hdf5)

(defparameter *DIM0* 4)


(cffi:defcstruct reading-t
  (temperature :double)
  (pressure    :double))


(cffi:defcstruct sensor-t
  (serial-no   :int)
  (location    :string)
  (temperature :double)
  (pressure    :double))


(defun create-reading-memtype ()
  (let ((result (h5tcreate :H5T-COMPOUND
			   (cffi:foreign-type-size '(:struct reading-t)))))
    (h5tinsert result "Temperature (F)"
	       (cffi:foreign-slot-offset '(:struct reading-t) 'temperature)
	       +H5T-NATIVE-DOUBLE+)
    (h5tinsert result "Pressure (inHg)"
	       (cffi:foreign-slot-offset '(:struct reading-t) 'pressure)
	       +H5T-NATIVE-DOUBLE+)
    result))


(defun create-sensor-memtype ()
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


(cffi:with-foreign-objects ((reading '(:struct sensor-t) *DIM0*)
			    (bkgrd '(:struct sensor-t) *DIM0*))  
  ;; Initialize data.
  (let ((bkgrd[0] (cffi:mem-aptr bkgrd '(:struct sensor-t) 0))
	(bkgrd[1] (cffi:mem-aptr bkgrd '(:struct sensor-t) 1))
	(bkgrd[2] (cffi:mem-aptr bkgrd '(:struct sensor-t) 2))
	(bkgrd[3] (cffi:mem-aptr bkgrd '(:struct sensor-t) 3))
	(reading[0] (cffi:mem-aptr reading '(:struct reading-t) 0))
	(reading[1] (cffi:mem-aptr reading '(:struct reading-t) 1))
	(reading[2] (cffi:mem-aptr reading '(:struct reading-t) 2))
	(reading[3] (cffi:mem-aptr reading '(:struct reading-t) 3)))
    
    (cffi:with-foreign-slots ((serial-no location temperature pressure)
			      bkgrd[0] (:struct sensor-t))
      (setf serial-no 1153 location "Exterior (static)"
            temperature 53.23d0 pressure 24.57d0))
    (cffi:with-foreign-slots ((serial-no location temperature pressure)
			      bkgrd[1] (:struct sensor-t))
      (setf serial-no 1184 location "Intake"
            temperature 55.12d0 pressure 22.95d0))
    (cffi:with-foreign-slots ((serial-no location temperature pressure)
			      bkgrd[2] (:struct sensor-t))
      (setf serial-no 1027 location "Intake manifold"
            temperature 103.55d0 pressure 31.23d0))
    (cffi:with-foreign-slots ((serial-no location temperature pressure)
			      bkgrd[3] (:struct sensor-t))
      (setf serial-no 1313 location "Exhaust manifold"
            temperature 1252.89d0 pressure 84.11d0))

    (cffi:with-foreign-slots ((temperature pressure)
                              reading[0] (:struct reading-t))
      (setf temperature 54.84d0 pressure 24.76d0))
    (cffi:with-foreign-slots ((temperature pressure)
			      reading[1] (:struct reading-t))
      (setf temperature 56.63d0 pressure 23.10d0))
    (cffi:with-foreign-slots ((temperature pressure)
			      reading[2] (:struct reading-t))
      (setf temperature 102.69d0 pressure 30.97d0))
    (cffi:with-foreign-slots ((temperature pressure)
			      reading[3] (:struct reading-t))
      (setf temperature 1238.27d0 pressure 82.15d0)))

  (let ((sourcetype (create-reading-memtype))
        (desttype (create-sensor-memtype)))

    ;; Convert the buffer in reading from sourcetype to desttype.
    ;; After this conversion we will use sensor to access the buffer,
    ;; as the buffer now matches its type.

    (h5tconvert sourcetype desttype *DIM0* reading bkgrd +H5P-DEFAULT+)

    ;; Output the data to the screen.

    (dotimes (i *DIM0*)
      (format t "sensor[~d]:~%" i)
      (let ((sensor[i] (cffi:mem-aptr reading '(:struct sensor-t) i)))
        (cffi:with-foreign-slots ((serial-no location temperature pressure)
                                  sensor[i] (:struct sensor-t))
          (format t "Serial number   : ~d~%" serial-no)
          (format t "Location        : ~a~%" location)
          (format t "Temperature (F) : ~6$~%" temperature)
          (format t "Pressure (inHg) : ~6$~%~%" pressure))))

    (h5ex:close-handles (list desttype sourcetype))))
