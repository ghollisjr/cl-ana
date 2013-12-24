;;;; package.lisp

(defpackage #:functional-utils 
  (:use :cl)
  (:export :flip
	   :to-pair-function
	   :lfuncall))
