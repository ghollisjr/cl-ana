;;;; package.lisp

(defpackage #:linear-algebra 
  (:use #:cl
	#:tensor
	#:list-utils)
  (:export :make-vector
	   :make-matrix
	   :vector->matrix
	   :euclidean-dot
	   :euclidean-norm
	   :euclidean-norm2
	   :matrix-transpose
           :matrix-mult
           :phi
           :theta))

(gmath:use-gmath :linear-algebra)
