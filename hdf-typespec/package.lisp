;;;; package.lisp

(defpackage #:hdf-typespec
  (:use :cl
	:cffi
	:list-utils
	:string-utils
	:memoization
	:hdf-cffi
	:typespec
	:alexandria)
  (:export :typespec-make-hdf-type
	   :hdf-type-make-typespec))
