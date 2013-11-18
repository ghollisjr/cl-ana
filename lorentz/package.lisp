;;;; package.lisp

(defpackage :lorentz
  (:use :cl
	:lisp-matrix
	:lisp-matrix-utils
	:xarray
	:iterate)
  (:export :lorentz-vector
	   :make-lorentz-vector
	   :make-double-float-vector
	   :lorentz-boost
	   :minkowski-norm
	   :minkowski-norm2
	   )) ; square of norm

(gmath:use-gmath :lorentz)
