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
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.hdf-table)

;;; NOTE: HDF5 types are left open forever.  I tried to implement a
;;; system of closing types and re-opening them on a table-by-table
;;; basis, but it turned out to be a complete hassle, so I'm leaving
;;; them open forever once opened at the moment.  In the future this
;;; could be revisited.  It poses only minimal memory overhead since
;;; I've memoized the typespec functions so that only one HDF5 type is
;;; used per typespec.  I believe the HDF5 type closing mechanism is
;;; for safety in situations where it isn't so easy to memoize
;;; functions.

(declaim (optimize (speed 2)
                   (safety 1)
                   (compilation-speed 0)
                   (debug 1)))

(defclass hdf-table (typed-table)
  ((row-buffer-size
    :initarg :buffer-size
    :initform nil
    :accessor hdf-table-buffer-size
    :documentation "buffer size in number of rows")
   (chunk-index
    :initarg :chunk-index
    :initform -1
    :accessor hdf-table-chunk-index
    :documentation "index to the in-memory chunk")
   (row-buffer
    :initarg :row-buffer
    :initform nil
    :accessor hdf-table-row-buffer
    :documentation "Object for storing a row to be read from/written
    to hdf table.  It's stored as part of the table for efficiency
    purposes and should not be handled directly by the user; use the
    awesome higher level tools for that.")
   (row-buffer-index
    :initarg :row-buffer-index
    :initform nil
    :accessor hdf-table-row-buffer-index
    :documentation "Index to the row in the buffer which is currently
    being modified prior to writing.")
   (hdf-dataset
    :initarg :hdf-dataset
    :initform nil
    :accessor hdf-table-dataset
    :documentation "hdf dataset which the table is reading from/writing
    to.")
   (hdf-row-type
    :initarg :hdf-row-type
    :initform nil
    :accessor hdf-table-row-type
    :documentation "hdf type for the row data object")
   (read-row-index
    :initarg :read-row-index
    :initform -1
    :accessor hdf-table-read-row-index
    :documentation "Index to row which should be sequentually read
    next")
   (nrows
    :initarg :nrows
    :initform nil
    :accessor hdf-table-nrows
    :documentation "number of rows in hdf-table")))

;; table-nrows method:
(defmethod table-nrows ((table hdf-table))
  (hdf-table-nrows table))

;; define high-level table access from files here
(defun open-hdf-table (hdf-file dataset-name &key buffer-size)
  (labels ((get-dataset-length (dataset)
             (with-open-dataspace (dataspace dataset)
               (let ((rank (h5sget-simple-extent-ndims dataspace)))
                 (with-foreign-objects ((dims 'hsize-t rank)
                                        (maxdims 'hsize-t rank))
                   (h5sget-simple-extent-dims dataspace dims maxdims)
                   (mem-aref dims 'hsize-t 0))))))
    (let* ((dataset (h5dopen2 hdf-file dataset-name +H5P-DEFAULT+))
           (typespec (dataset-read-typespec dataset))
           (cstruct (typespec->cffi-type typespec))
           (hdf-row-type (h5dget-type dataset))
           (buffer-size
            (if (null buffer-size)
                ;; get buffer-size from the dataset
                ;; chunk-size:
                ;; this needs cleaning up
                (let* ((create-plist (h5dget-create-plist dataset)))
                  (with-foreign-object (chunkdims 'hsize-t 1)
                    ;; this needs cleaning up
                    (h5pget-chunk create-plist
                                  1
                                  chunkdims)
                    (mem-aref chunkdims 'hsize-t 0)))
                buffer-size))
           (row-buffer
            (typespec-foreign-alloc typespec buffer-size)))
      (make-instance 'hdf-table
                     :field-names (typespec->field-names typespec)
                     :field-specs (typespec->field-specs typespec)
                     :row-buffer row-buffer
                     :row-pointer
                     (mem-aptr row-buffer
                               cstruct
                               0)
                     :hdf-dataset dataset
                     :hdf-row-type hdf-row-type
                     :row-cstruct cstruct
                     :buffer-size buffer-size
                     :access-mode :read
                     :nrows
                     (get-dataset-length dataset)))))

(defun create-hdf-table
    (hdf-file dataset-path names-specs &key (buffer-size 1000))
  "Creates a hdf-table for writing in hdf-file with dataset-path as
  the path to the dataset in the hdf-file and the alist names-specs
  which maps the field names to their typespecs (this is just
  applying rest to the typespec for the table).  Buffer size will be
  used as both the chunksize for the hdf dataset and as the size of
  the buffer for writing into the file."
  (let* ((typespec (cons :compound names-specs))
	 (cstruct (typespec->cffi-type typespec))
	 (row-buffer ;; (foreign-alloc cstruct
          ;;    	    :count
          ;;    	    buffer-size)
          (typespec-foreign-alloc typespec buffer-size))
	 (hdf-type (typespec->hdf-type typespec))
	 (dataset
	  (progn
	    (let (cparms)
	      (with-foreign-objects
		  ((chunkdims 'hsize-t 1)
		   (dims 'hsize-t 1)
		   (maxdims 'hsize-t 1))
		(setf cparms (h5pcreate +H5P-DATASET-CREATE+))
		(setf (mem-aref chunkdims 'hsize-t 0) buffer-size)
		(setf (mem-aref dims 'hsize-t 0) 0)
		(setf (mem-aref maxdims 'hsize-t 0) +H5S-UNLIMITED+)
		(h5pset-chunk cparms 1 chunkdims)
                ;; this needs cleaning up
		(with-create-dataspace (dataspace 1 dims maxdims)
                  (h5dcreate1 hdf-file
                              dataset-path
                              hdf-type
                              dataspace
                              cparms)))))))
    (make-instance 'hdf-table
		   :row-cstruct cstruct
                   :row-pointer
                   (mem-aptr row-buffer
                             cstruct
                             0)
                   :field-names (mapcar #'car names-specs)
                   :field-specs
                   (mapcar #'cdr names-specs)
		   :row-buffer row-buffer
		   :buffer-size buffer-size
		   :chunk-index 0
		   :row-buffer-index 0
		   :hdf-row-type hdf-type
		   :hdf-dataset dataset
		   :access-mode :write)))

(defmethod table-close ((table hdf-table))
  "Cleanup function only to be called on an hdf-table for writing.
Writes the last remaining data in the buffer to the file and closes
the dataset."
  (with-accessors ((dataset hdf-table-dataset)
		   (row-buffer-index hdf-table-row-buffer-index)
		   (hdf-type hdf-table-row-type)
                   (fields-specs table-field-specs)
		   (chunk-index hdf-table-chunk-index)
		   (row-buffer hdf-table-row-buffer)
		   (buffer-size hdf-table-buffer-size)
		   (cstruct typed-table-row-cstruct)
		   (access-mode table-access-mode))
      table
    (when (equal access-mode :write)
      (when (not (zerop row-buffer-index))
        (with-foreign-objects ((start 'hsize-t 1)
                               (stride 'hsize-t 1)
                               (count 'hsize-t 1)
                               (blck 'hsize-t 1)
                               (dataspace-dims 'hsize-t 1)
                               (dataspace-maxdims 'hsize-t 1)
                               (memdims 'hsize-t 1)
                               (memmaxdims 'hsize-t 1))
          (setf (mem-aref start 'hsize-t 0)
                (* buffer-size chunk-index))
          (setf (mem-aref stride 'hsize-t 0) 1)
          (setf (mem-aref count 'hsize-t 0) 1)
          (setf (mem-aref blck 'hsize-t 0) row-buffer-index)
          (with-open-dataspace (dataspace dataset)
            (h5sget-simple-extent-dims
             dataspace dataspace-dims dataspace-maxdims)
            (incf (mem-aref dataspace-dims 'hsize-t 0)
                  row-buffer-index)
            (h5dset-extent dataset dataspace-dims))
          (with-open-dataspace (dataspace dataset)
            (setf (mem-aref memdims 'hsize-t 0) row-buffer-index)
            (setf (mem-aref memmaxdims 'hsize-t 0) row-buffer-index)
            (with-create-dataspace (memspace 1 memdims memmaxdims)
              (h5sselect-hyperslab
               dataspace :H5S-SELECT-SET start stride count blck)
              (h5dwrite
               dataset hdf-type memspace
               dataspace +H5P-DEFAULT+ row-buffer))))))
    ;; no type closing for now, it's a complete headache and is
    ;; causing problems with memoization etc.
    
    ;; (let ((typespec (cons :compound fields-specs)))
    ;;   (print 'typespec->hdf-type)
    ;;   (print (typespec->hdf-type :int))
    ;;   (remhash (list typespec) (get-memo-map #'typespec->hdf-type)))
    ;; (print 'hdf-type->typespec)
    ;; (remhash (list hdf-type) (get-memo-map #'hdf-type->typespec))
    (foreign-free row-buffer)))

(defmethod table-commit-row ((table hdf-table))
  (with-accessors ((row-buffer hdf-table-row-buffer)
                   (row-pointer typed-table-row-pointer)
		   (row-buffer-index hdf-table-row-buffer-index)
		   (buffer-size hdf-table-buffer-size)
		   (chunk-index hdf-table-chunk-index)
		   (cstruct typed-table-row-cstruct)
		   (dataset hdf-table-dataset)
		   (hdf-type hdf-table-row-type))
      table
    (incf row-buffer-index)
    (setf row-pointer
          (mem-aptr row-buffer
                    cstruct
                    (mod row-buffer-index buffer-size)))
    (when (zerop (mod row-buffer-index buffer-size)) ;; was rem
      (with-foreign-objects ((start 'hsize-t 1)
                             (stride 'hsize-t 1)
                             (count 'hsize-t 1)
                             (blck 'hsize-t 1)
                             (dataspace-dims 'hsize-t 1)
                             (dataspace-maxdims 'hsize-t 1)
                             (memdims 'hsize-t 1)
                             (memmaxdims 'hsize-t 1))
        (with-open-dataspace (dataspace dataset)
          (h5sget-simple-extent-dims
           dataspace dataspace-dims dataspace-maxdims)
          (incf (mem-aref dataspace-dims 'hsize-t 0) buffer-size)
          (h5dset-extent dataset dataspace-dims))
        (with-open-dataspace (dataspace dataset)
          (setf (mem-aref start 'hsize-t 0)
                (* buffer-size chunk-index))
          (setf (mem-aref stride 'hsize-t 0) 1)
          (setf (mem-aref count 'hsize-t 0) 1)
          (setf (mem-aref blck 'hsize-t 0) buffer-size)
          (setf (mem-aref memdims 'hsize-t 0) buffer-size)
          (setf (mem-aref memmaxdims 'hsize-t 0) buffer-size)
          (with-create-dataspace (memspace 1 memdims memmaxdims)
            (h5sselect-hyperslab dataspace
                                 :H5S-SELECT-SET
                                 start stride count blck)
            (h5dwrite dataset hdf-type
                      memspace dataspace
                      +H5P-DEFAULT+ row-buffer))))
      (incf chunk-index)
      (setf row-buffer-index 0))))

(defun table->cstruct (table)
  "Function which, given an hdf-table, defines a cffi cstruct for use
  in reading/writing from the hdf file."
  (typespec->cffi-type (typed-table->typespec table)))

(defun table->hdf-type (table)
  (typespec->hdf-type (typed-table->typespec table)))

(defun dataset-read-typespec (dataset)
  "Reads the typespec from the dataset"
  ;; this needs cleaning up
  (with-dataset-type (type dataset)
    (hdf-type->typespec type)))

;; use foreign-slot-value to get the value of a slot

;; use foreign-alloc to create a foreign object; should use
;; foreign-free once the object is not needed

;; assumes:
;;
;; * The cstruct row object has already been created
;;
;; * The dataset has already been opened and its parameters set
;;
;; * The memory dataspace has already been created and its parameters set
;;
;; * The row object hdf type has been created

(defmethod table-load-next-row ((table hdf-table))
  (with-accessors ((row-number hdf-table-read-row-index)
                   (row-buffer hdf-table-row-buffer)
                   (row-pointer typed-table-row-pointer)
                   (buffer-size hdf-table-buffer-size)
                   (cstruct typed-table-row-cstruct))
      table
    ;;(print row-number)
    (incf row-number)
    (setf row-pointer
          (mem-aptr row-buffer
                    cstruct
                    (mod row-number buffer-size)))
    (load-chunk table row-number)))

(defun load-chunk (table row-number)
  (labels ((get-chunk-index (row-index buffer-size)
             (declare (fixnum buffer-size row-index))
	     (the fixnum
                  (floor row-index buffer-size))))
    (with-accessors ((buffer-size hdf-table-buffer-size)
		     (current-chunk-index hdf-table-chunk-index)
		     (row-buffer hdf-table-row-buffer)
		     (cstruct typed-table-row-cstruct)
                     (nrows hdf-table-nrows))
	table
      (let ((chunk-index (get-chunk-index row-number buffer-size)))
        (if (< row-number nrows)
            (progn
              (when (not (= current-chunk-index chunk-index))
                ;; read data from file:
                (with-accessors ((row-hdf-type hdf-table-row-type)
                                 (dataset hdf-table-dataset)
                                 (table-length hdf-table-nrows))
                    table
                  (let ((chunk-size (min (- table-length
                                            (* chunk-index
                                               buffer-size))
                                         buffer-size)))
                    (with-foreign-objects ((start 'hsize-t 1)
                                           (stride 'hsize-t 1)
                                           (count 'hsize-t 1)
                                           (blck 'hsize-t 1)
                                           (memdims 'hsize-t 1)
                                           (memmaxdims 'hsize-t 1))
                      (setf (mem-aref start 'hsize-t 0)
                            (* chunk-index buffer-size))
                      (setf (mem-aref stride 'hsize-t 0) 1)
                      (setf (mem-aref count 'hsize-t 0) 1)
                      (setf (mem-aref blck 'hsize-t 0) chunk-size)
                      (setf (mem-aref memdims 'hsize-t 0) chunk-size)
                      (setf (mem-aref memmaxdims 'hsize-t 0) chunk-size)
                      (with-open-dataspace (dataspace dataset)
                        (h5sselect-hyperslab dataspace
                                             :H5S-SELECT-SET
                                             start
                                             stride
                                             count
                                             blck)
                        (with-create-dataspace
                            (memspace 1 memdims memmaxdims)
                          (h5dread dataset
                                   row-hdf-type
                                   memspace
                                   dataspace
                                   +H5P-DEFAULT+
                                   row-buffer))
                        (setf (hdf-table-row-buffer table) row-buffer)
                        (setf (hdf-table-chunk-index table) chunk-index))))))
              t)
            nil)))))
