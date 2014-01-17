;;;; package.lisp

(defpackage #:hdf-typespec 
  (:use :cl
	:cffi
	:list-utils
	:string-utils
        :symbol-utils
	:memoization
	:hdf-cffi
	:typespec
	:alexandria)
  (:export :typespec->hdf-type
	   :hdf-type->typespec))
