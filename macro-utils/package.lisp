;;;; package.lisp

(defpackage #:macro-utils
  (:use #:cl
	#:alexandria)
  (:export :inrange
	   :cond-setf
	   :print-eval))
