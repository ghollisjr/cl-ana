;;;; package.lisp

(defpackage :lorentz
  (:use :cl
	:linear-algebra
	:xarray
	:iterate)
  (:export :lorentz-vector
	   :make-lorentz-vector
	   :make-double-float-vector
	   :lorentz-boost
	   :minkowski-norm
	   :minkowski-norm2)) ; minkowski-norm^2

(gmath:use-gmath :lorentz)
