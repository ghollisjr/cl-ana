;;;; package.lisp

(defpackage #:gnuplot-i-cffi
  (:use :cl
	:cffi)
  (:export :gnuplot-init
	   :gnuplot-cmd))
