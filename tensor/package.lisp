;;;; package.lisp

(defpackage #:tensor
  (:use #:cl
	#:list-utils
	#:alexandria)
  (:export :make-tensor
	   :tensor-ref
	   :tensor-map
	   :tensor-+
	   :tensor--
	   :tensor-*
	   :tensor-/
	   :tensor-rank
	   :tensor-dimensions
	   :sequencep
	   :tensor-contract))

(gmath:use-gmath :tensor)
