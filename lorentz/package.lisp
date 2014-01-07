;;;; package.lisp

(defpackage :lorentz 
  (:use :cl
	:linear-algebra
	:tensor
	:iterate)
  (:export :lorentz-vector
	   :make-lorentz-vector
	   :make-double-float-vector
	   :lorentz-boost
           :lorentz-vector-spatial
           :minkowski-dot
	   :minkowski-norm
	   :minkowski-norm2)) ; minkowski-norm^2

(gmath:use-gmath :lorentz)
