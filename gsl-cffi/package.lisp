;;;; package.lisp

(defpackage #:gsl-cffi
  (:use :cl
	:cffi)
  (:export :gsl-ntuple-read
	   :+GSL-EOF+))
	   ;;:gsl-multifit-fsolver-driver))
