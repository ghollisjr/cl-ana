;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to track links in a group by
;;; creation order.  The program creates a series of groups,
;;; then reads back their names: first in alphabetical order,
;;; then in creation order.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5G/h5ex_g_corder.c





(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_g_corder.h5" *load-pathname*)))

(cffi:with-foreign-object (ginfo '(:struct h5g-info-t) 1)

  ;; Create a new file using the default properties.

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))

    (unwind-protect
	 ;; Create group creation property list and enable link creation
	 ;; order tracking.  Attempting to track by creation order in a
	 ;; group that does not have this property set will result in an
	 ;; error.
	 (let* ((gcpl (h5pcreate +H5P-GROUP-CREATE+))
		(group (prog2 (h5pset-link-creation-order
			       gcpl (logior +H5P-CRT-ORDER-TRACKED+
					    +H5P-CRT-ORDER-INDEXED+))
			   (h5gcreate2 file "index_group" +H5P-DEFAULT+
				       gcpl +H5P-DEFAULT+))))

	   ;; Create subgroups in the primary group.  These will be tracked
	   ;; by creation order.  Note that these groups do not have to have
	   ;; the creation order tracking property set.

	   (h5ex:close-handles (list (h5gcreate2 group "H" +H5P-DEFAULT+
						 +H5P-DEFAULT+ +H5P-DEFAULT+)
				     (h5gcreate2 group "D" +H5P-DEFAULT+
						 +H5P-DEFAULT+ +H5P-DEFAULT+)
				     (h5gcreate2 group "F" +H5P-DEFAULT+
						 +H5P-DEFAULT+ +H5P-DEFAULT+)
				     (h5gcreate2 group "5" +H5P-DEFAULT+
						 +H5P-DEFAULT+ +H5P-DEFAULT+)))
	   (h5gget-info group ginfo)

	   ;; Traverse links in the primary group using alphabetical indices
	   ;; (H5_INDEX_NAME).

	   (format t "Traversing group using alphabetical indices:~%~%")
	   (cffi:with-foreign-slots ((nlinks) ginfo (:struct h5g-info-t))
	     (dotimes (i nlinks)
	       ;; Get size of name, add 1 for null terminator.
	       ;; Allocate storage for name.
	       (let* ((size (1+ (h5lget-name-by-idx group "."
						    :H5-INDEX-NAME
						    :H5-ITER-INC i +NULL+ 0
						    +H5P-DEFAULT+)))
		      (name (cffi:foreign-alloc :char :count size)))

		 ;; Retrieve name, print it, and free the previously
		 ;; allocated space.
		 (h5lget-name-by-idx group "." :H5-INDEX-NAME
				     :H5-ITER-INC i name size +H5P-DEFAULT+)
		 (format t "Index ~a: ~a~%" i
			 (cffi:foreign-string-to-lisp name))
		 (cffi:foreign-free name)))

	     ;; Traverse links in the primary group by creation order
	     ;; (H5_INDEX_CRT_ORDER).

	     (format t "~%Traversing group using creation order indices:~%~%")
	     (dotimes (i nlinks)
	       ;; Get size of name, add 1 for null terminator.
	       ;; Allocate storage for name.
	       (let* ((size (1+ (h5lget-name-by-idx group "."
						    :H5-INDEX-CRT-ORDER
						    :H5-ITER-INC i +NULL+ 0
						    +H5P-DEFAULT+)))
		      (name (cffi:foreign-alloc :char :count size)))

		 ;; Retrieve name, print it, and free the previously
		 ;; allocated space.
		 (h5lget-name-by-idx group "." :H5-INDEX-CRT-ORDER
				     :H5-ITER-INC i name size +H5P-DEFAULT+)
		 (format t "Index ~a: ~a~%" i
			 (cffi:foreign-string-to-lisp name))
		 (cffi:foreign-free name))))

	   (h5ex:close-handles (list group gcpl)))
      (h5ex:close-handles (list file fapl)))))
