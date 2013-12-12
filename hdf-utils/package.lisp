;;;; package.lisp

(defpackage #:hdf-utils
  (:use #:cl
	#:cffi
	#:hdf-cffi
        #:alexandria)
  (:export :with-cleanup          ; general use for HDF5 objects
           :with-open-dataspace   ; opens a dataspace from a dataset
           :with-create-dataspace ; creates a new dataspace
           :with-dataset-type     ; opens dataset datatype
           :with-open-hdf-file    ; opens/creates a file for reading/writing
	   :open-hdf-file
	   :close-hdf-file))
