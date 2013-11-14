;;;; package.lisp

(defpackage #:typespec
  (:use :cl
	:list-utils
	:string-utils
	:memoization
	:cffi)
  (:export :typespec-make-cstruct))
