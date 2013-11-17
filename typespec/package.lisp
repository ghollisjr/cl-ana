;;;; package.lisp

(defpackage #:typespec
  (:use :cl
	:list-utils
	:string-utils
	:memoization
	:cffi)
  (:export :typespec->cffi-type
	   :typespec-compound-p
	   :typespec-array-p
	   :typespec-flatten-arrays))
