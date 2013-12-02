;;;; package.lisp

(defpackage #:error-propogation
  (:nicknames #:err-prop)
  (:use :cl)
  (:export :*err-num-pretty-print* ; for print style
	   :err-num
	   :make-err-num
	   :err-num-value
	   :err-num-error
	   ;; These functions are provided in the event that one wants
	   ;; to supply a lot of arguments to the generic +, -, *, or
	   ;; / functions, since this would be inefficient the way it
	   ;; is implemented currently:
	   :err-num-+
	   :err-num--
	   :err-num-*
	   :err-num-/))

(gmath:use-gmath :error-propogation)
