;;;; package.lisp

(defpackage #:hdf-file
  (:use #:cl
	#:cffi
	#:hdf-cffi)
  (:export :with-open-hdf-file
	   :open-hdf-file
	   :close-hdf-file))
