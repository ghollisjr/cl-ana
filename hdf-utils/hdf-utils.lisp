;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
;;;;
;;;; This file is part of cl-ana.
;;;;
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.hdf-utils)

;;;; Still need to fix the with-open-hdf-file macro to make use of the
;;;; open-hdf-file and close-hdf-file functions.

(declaim (optimize (speed 3)
                   (safety 1)
                   (compilation-speed 0)
                   (debug 1)))

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
  (let ((filename
         (namestring (->absolute-pathname filename))))
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
                (delete-file filename)
                (apply #'h5fcreate filename +H5F-ACC-TRUNC+ write-access-parameters))
               (:rename
                (progn
                  ;; rename "file" to "file.bak"
                  ;; then create new file
                  (rename-file filename (concatenate 'string filename ".bak"))
                  (apply #'h5fcreate filename +H5F-ACC-TRUNC+ write-access-parameters)))
               (:append
                (h5fopen filename +H5F-ACC-RDWR+ read-access-parameters)))
	     (case if-does-not-exist
	       (:create
		(apply #'h5fcreate filename +H5F-ACC-TRUNC+ write-access-parameters))
	       (:error (error "file does not exist"))
	       (nil nil))))))))

(defun close-hdf-file (hdf-file)
  "Closes all datatype objects associated with file along with the
file"
  (reset-memo-map #'typespec->hdf-type)
  (reset-memo-map #'hdf-type->typespec)
  (let ((nobs (h5fget-obj-count hdf-file +H5F-OBJ-ALL+))
        (ids nil))
    (when (plusp nobs)
      (with-foreign-object (raw-ids 'hid-t nobs)
        (h5fget-obj-ids hdf-file
                        +H5F-OBJ-ALL+
                        nobs
                        raw-ids)
        (setf ids
              (loop
                 for i below nobs
                 collecting (mem-aref raw-ids 'hid-t i)))))
    (loop
       for id in ids
       do
       ;; In the future, detect type instead of trying everything
         (h5fclose id)
         (h5dclose id)
         (h5gclose id)
         (h5tclose id)
         (h5aclose id)))
  (h5fclose hdf-file))

(defmacro with-open-hdf-file ((hdf-file
			       file-path-or-string
			       &key
			       direction
			       (if-exists nil if-exists-supplied-p)
			       (if-does-not-exist nil if-does-not-exist-supplied-p)
			       (read-access-parameters +H5P-DEFAULT+)
			       (write-access-parameters (list +H5P-DEFAULT+ +H5P-DEFAULT+)))
			      &body body)

  "Macro providing lispy access to hdf (HDF5) files.  Use just like
you would with-open-file, just give a file and a path/string.

The usual key arguments for access mode, etc. are honored and
transformed into HDF5 terms.

The argument hdf-file is the symbol you'll use to refer to the file
handle inside of the macro body."

  ;; direction: :input :output
  ;; if-exists: :error :supersede nil :rename
  ;; if-does-not-exist: :error :create nil

  (let* ((result (gensym))
	 (body-with-close
	  `(let ((,result
		  (progn ,@body)))
	     (close-hdf-file ,hdf-file)
	     ,result)))
    `(let ((,hdf-file
            (open-hdf-file ,file-path-or-string
                           ,@(when-keywords
                              direction
                              if-exists
                              if-does-not-exist
                              read-access-parameters
                              (:write-access-parameters
                               (cons 'list write-access-parameters))))))
       ,body-with-close)))

(defun hdf-mkgroup (file group-name)
  "Creates a group with name group-name in hdf-file file; returns path
to group with final '/' included."
  (let ((group-name
         (if (not (equal (last-elt group-name) #\/))
             group-name
             (subseq group-name 0 (1- (length group-name))))))
    (h5gclose (h5gcreate1 file group-name 0))
    (string-append group-name "/")))
