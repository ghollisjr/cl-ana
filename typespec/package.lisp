;;;; package.lisp

(defpackage #:typespec
  (:use :cl
	:list-utils
	:string-utils
	:memoization
        :alexandria
	:cffi)
  (:export :typespec->cffi-type
           :compound-typespec-alloc
	   :typespec-compound-p
	   :typespec-array-p
	   :typespec-flatten-arrays))
