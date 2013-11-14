;;;; hdf-typespec.lisp

(in-package :hdf-typespec)

;; defines the structure as an hdf type if necessary from the typespec.
(defun-memoized typespec-make-hdf-type (typespec)
  (if (listp typespec)
      ;; handle compound and array types
      (case (first typespec)
	;; array typespec: (:array type rank dim-list)
	(:array
	 (let ((type (typespec-make-hdf-type (second typespec)))
	       (rank (third typespec))
	       (dim-list (fourth typespec)))
	   (with-foreign-object (dims 'hsize-t rank)
	     (loop for d in dim-list for i from 0
	       do (setf (mem-aref dims :uint i) d))
	     (h5tarray-create2 type rank dims))))
	;; compound typespec: (:compound ("name1" . type1) ...)
	(:compound
	 (let* ((names-specs (rest typespec))
		(names (mapcar #'car names-specs))
		(specs (mapcar #'cdr names-specs))
		(hdf-types (mapcar #'typespec-make-hdf-type specs))
		(slot-symbols (mapcar (compose #'intern #'lispify)
				      names))
		(cstruct (typespec-make-cstruct typespec))
		(offsets (mapcar #'(lambda (x) (foreign-slot-offset cstruct x))
				 slot-symbols))
		(compound-tid (h5tcreate +H5T-COMPOUND+ (foreign-type-size cstruct))))
	   (loop
	      for name in names
	      for offset in offsets
	      for type in hdf-types
	      do (h5tinsert compound-tid name offset type))
	   compound-tid)))
      ;; return the hdf type corresponding to the cffi type:
      (hdf-native-type typespec)))

;; Construct typespec from hdf type:
(defun-memoized hdf-type-make-typespec (hdf-type)
  (let ((hdf-class (h5tget-class hdf-type)))
    (case hdf-class
      (:H5T-INTEGER
       (cffi-native-type (h5tget-native-type hdf-type :H5T-DIR-DEFAULT)))
      (:H5T-FLOAT
       (cffi-native-type (h5tget-native-type hdf-type :H5T-DIR-DEFAULT)))
      (:H5T-ARRAY
       (let* ((base-type (h5tget-super hdf-type))
	      (native-base-type (h5tget-native-type base-type :H5T-DIR-DEFAULT))
	      (array-rank (h5tget-array-ndims hdf-type)))
	 (with-foreign-object (array-dims-pointer 'hsize-t array-rank)
	   (h5tget-array-dims2 hdf-type array-dims-pointer)
	   (let (array-dims)
	     (dotimes (index array-rank)
	       (push (mem-aref array-dims-pointer 'hsize-t index)
		     array-dims))
	     (list :array
		   (cffi-native-type native-base-type)
		   array-rank array-dims)))))
      (:H5T-COMPOUND
       (let* ((nmembers (h5tget-nmembers hdf-type))
	      (names
	       (loop for i from 0 to (1- nmembers)
		    collecting (h5tget-member-name hdf-type i)))
	      (member-typespecs
	       (loop for i from 0 to (1- nmembers)
		    collecting (hdf-type-make-typespec
				(h5tget-member-type hdf-type i))))
	      (names-specs (zip names member-typespecs)))
	 (cons :compound names-specs))))))
