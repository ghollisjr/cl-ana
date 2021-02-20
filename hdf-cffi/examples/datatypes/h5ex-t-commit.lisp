;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.


;;; This example shows how to commit a named datatype to a
;;; file, and read back that datatype.  The program first
;;; defines a compound datatype, commits it to a file, then
;;; closes the file.  Next, it reopens the file, opens the
;;; datatype, and outputs the names of its fields to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_commit.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_commit.h5" *load-pathname*)))
(defparameter *DATATYPE* "Sensor_Type")

(defun create-filetype ()

  ;; Create the compound datatype.  Because the standard types we are
  ;; using may have different sizes than the corresponding native
  ;; types, we must manually calculate the offset of each member.
  
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


;; Create a new file using the default properties.
(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((filetype (create-filetype)))
         (h5tcommit2 file *DATATYPE* filetype +H5P-DEFAULT+ +H5P-DEFAULT+
                    +H5P-DEFAULT+)
         ;; Close and release resources.
         (h5tclose filetype))
    (h5ex:close-handles (list file fapl))))

;; Now we begin the read section of this example.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       (let* ((filetype (h5topen2 file *DATATYPE* +H5P-DEFAULT+))
              (typeclass (h5tget-class filetype)))

         ;; Output the data to the screen.
         (format t "Named datatype ~a:~%" *DATATYPE*)

         (if (equal typeclass :H5T-COMPOUND)
             (progn
               (format t "   Class: H5T_COMPOUND~%")
               (let ((nmembs (h5tget-nmembers filetype)))
                 (dotimes (i nmembs)
                   ;; Get the member name and print it.  Note that
                   ;; H5Tget_member_name allocates space for the string in
                   ;; name, so we must free() it after use.
                   (let ((name (h5tget-member-name filetype i)))
                     (format t "   ~a~%" (cffi:foreign-string-to-lisp name))
                     (cffi:foreign-free name))))))
         (h5tclose filetype))
    (h5ex:close-handles (list file fapl))))
