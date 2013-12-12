;;;; hdf-utils.lisp

(in-package :hdf-utils)

;;;; Still need to fix the with-open-hdf-file macro to make use of the
;;;; open-hdf-file and close-hdf-file functions.

(declaim (optimize (speed 3)
                   (safety 0)
                   (compilation-speed 0)
                   (debug 0)))

(defmacro with-cleanup ((&rest specs) &body body)
  "Executes body inside of a lexical environment where objects
described by specs are bound and then freed upon concluding the body.

Each spec is a list of 3 elements:

1. A symbol which will denote a variable inside the body,
2. A form which will be evaluated and its value bound to the symbol,
3. A function for cleaning up the object referenced by the symbol; it
will be given the symbol as the sole argument."
  (multiple-value-bind (symbols inits cleanup-fns)
      (loop
         for (s i c) in specs
         collecting s into symbols
         collecting i into inits
         collecting c into cleanup-fns
         finally (return (values symbols inits cleanup-fns)))
    (let ((lexical-bindings
           (loop
              for s in symbols
              for i in inits
              collecting `(,s ,i)))
          (cleanup-forms
           (loop
              for s in symbols
              for c in cleanup-fns
              collecting `(funcall ,c ,s))))
      (with-gensyms (result)
        `(let ,lexical-bindings
           (let ((,result
                  (progn ,@body)))
             ,@(reverse cleanup-forms)
             ,result))))))

(defmacro with-open-dataspace ((dataspace dataset) &body body)
  "Safely work with dataspace taken from dataset"
  `(with-cleanup
       ((,dataspace (h5dget-space ,dataset)
                    #'h5sclose))
     ,@body))

(defmacro with-create-dataspace ((dataspace rank dims maxdims)
                                 &body body)
  "Creates a new dataspace and cleans up after it"
  `(with-cleanup
       ((,dataspace (h5screate-simple ,rank ,dims ,maxdims)
                    #'h5sclose))
     ,@body))

(defmacro with-dataset-type ((type dataset) &body body)
  `(with-cleanup
       ((,type (h5dget-type ,dataset)
               #'h5tclose))
     ,@body))

;;;; File utilities:

(defun open-hdf-file
    (filename &key direction
		(if-exists nil if-exists-supplied-p)
		(if-does-not-exist nil if-does-not-exist-supplied-p)
		(read-access-parameters +H5P-DEFAULT+)
		(write-access-parameters (list +H5P-DEFAULT+ +H5P-DEFAULT+)))
  "Opens an hdf file, returning a handle to the file (hid-t).

direction can be :input or :output.

if-exists can be :supersede :error nil

if-does-not-exist can be :create :error nil"
  (case direction
    (:input
     ;; I'm choosing the convention that when direction is :input, the
     ;; only meaningful if-does-not-exist are :error and nil
       ;; setup if-does-not-exist:
     (when (not if-does-not-exist-supplied-p)
       (setf if-does-not-exist :error))
     (let ((file-exists-p (probe-file filename)))
       (if file-exists-p
	   (h5fopen filename +H5F-ACC-RDONLY+ read-access-parameters)
	   (case if-does-not-exist
	     (:error (error "File does not exist"))
	     (nil nil)))))
    (:output
     ;; default for if-exists is :error
     ;; can't ignore if-does-not-exist if I want to be consistent with
     ;; with-open-file, but default is to go ahead and create the file
     
       ;; setup if-exists & if-does-not-exist
     (when (not if-exists-supplied-p)
	 (setf if-exists :error))
       (when (not if-does-not-exist-supplied-p)
	 (setf if-does-not-exist :create))
       (let ((file-exists-p (probe-file filename)))
	 (if file-exists-p
	     (case if-exists
		(:error (error "File exists"))
		(nil nil)
		(:supersede
		 (apply #'h5fcreate filename +H5F-ACC-TRUNC+ write-access-parameters))
		(:rename
		 (progn
		   ;; rename "file" to "file.bak"
		   ;; then create new file
		   (rename-file filename (concatenate 'string filename ".bak"))
		   (apply #'h5fcreate filename +H5F-ACC-TRUNC+ write-access-parameters))))
	     (case if-does-not-exist
	       (:create
		(apply #'h5fcreate filename +H5F-ACC-TRUNC+ write-access-parameters))
	       (:error (error "file does not exist"))
	       (nil nil)))))))

(defun close-hdf-file (hdf-file)
  "Just a wrapper around the h5fclose function"
  (h5fclose hdf-file))

(defmacro with-open-hdf-file ((hdf-file
			       file-string ;; must be a string due to cffi
			       &key
			       direction
			       (if-exists nil if-exists-supplied-p)
			       (if-does-not-exist nil if-does-not-exist-supplied-p)
			       (read-access-parameters +H5P-DEFAULT+)
			       (write-access-parameters (list +H5P-DEFAULT+ +H5P-DEFAULT+)))
			      &body body)

  "Macro providing lispy access to hdf (HDF5) files.  Use (almost)
just like you would with-open-file, just give a file and a
string (must be an actual string, not a pathname).

The usual key arguments for access mode, etc. are honored and
transformed into HDF5 terms.

The argument hdf-file is the symbol you'll use to refer to the file
handle inside of the macro body."

  ;; direction: :input :output
  ;; if-exists: :error :supersede nil :rename
  ;; if-does-not-exist: :error :create nil

  ;; (body-with-close (append body `((h5fclose ,hdf-file))))
  (let* ((result (gensym))
	 (body-with-close
	  `(let ((,result
		  (progn ,@body)))
	     (h5fclose ,hdf-file)
	     ,result)))
    (case direction
      (:input
       ;; I'm choosing the convention that when direction is :input, the
       ;; only meaningful if-does-not-exist are :error and nil
       (let ((file-string-foreign (gensym)))
	 ;; setup if-does-not-exist:
	 (when (not if-does-not-exist-supplied-p)
	   (setf if-does-not-exist :error))
	 `(let ((file-exists-p (probe-file ,file-string)))
	    (if file-exists-p
		(let ((,hdf-file
		       (with-foreign-string (,file-string-foreign ,file-string)
			 (h5fopen ,file-string-foreign +H5F-ACC-RDONLY+ ,read-access-parameters))))
		  ,body-with-close)
		(case ,if-does-not-exist
		  (:error (error "File does not exist"))
		  (nil (let ((,hdf-file nil))
			 ,@body)))))))
      (:output
       ;; default for if-exists is :error
       ;; can't ignore if-does-not-exist if I want to be consistent with
       ;; with-open-file, but default is to go ahead and create the file
       
       (let ((file-string-foreign (gensym)))
	 ;; setup if-exists & if-does-not-exist
	 (when (not if-exists-supplied-p)
	   (setf if-exists :error))
	 (when (not if-does-not-exist-supplied-p)
	   (setf if-does-not-exist :create))
	 `(let ((file-exists-p (probe-file ,file-string)))
	    (if file-exists-p
		(case ,if-exists
		  (:error (error "File exists"))
		  (nil (let ((,hdf-file nil))
			 ,@body))
		  (:supersede (let ((,hdf-file
				     (with-foreign-string (,file-string-foreign ,file-string)
				       (h5fcreate ,file-string-foreign +H5F-ACC-TRUNC+ ,@write-access-parameters))))
				,body-with-close))
		  (:rename (progn
			     ;; rename "file" to "file.bak"
			     ;; then create new file
			     (rename-file ,file-string (concatenate 'string ,file-string ".bak"))
			     (let ((,hdf-file
				    (with-foreign-string (,file-string-foreign ,file-string)
				      (h5fcreate ,file-string-foreign +H5F-ACC-TRUNC+ ,@write-access-parameters))))
			       ,body-with-close))))
		(case ,if-does-not-exist
		  (:create (let ((,hdf-file
				  (with-foreign-string (,file-string-foreign ,file-string)
				    (h5fcreate ,file-string-foreign +H5F-ACC-TRUNC+ ,@write-access-parameters))))
			     ,body-with-close))
		  (:error (error "file does not exist"))
		  (nil (let ((,hdf-file nil))
			 ,@body))))))))))
