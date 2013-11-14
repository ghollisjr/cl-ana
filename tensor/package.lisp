;;;; package.lisp

(defpackage #:tensor
  (:use #:cl
	#:alexandria)
  (:export :tensor-map
	   :tensor-+
	   :tensor--
	   :tensor-*
	   :tensor-/))

(gmath:use-gmath :tensor)
