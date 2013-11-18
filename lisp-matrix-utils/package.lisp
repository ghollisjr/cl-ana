;;;; package.lisp

(defpackage #:lisp-matrix-utils
  (:use :cl
	:lisp-matrix)
  (:export :make-double-float-vector
	   :euclidean-norm
	   :euclidean-norm2))
