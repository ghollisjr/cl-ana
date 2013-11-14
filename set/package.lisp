;;;; package.lisp

(defpackage #:set
  (:use :cl
	:functional-utils)
  (:export :make-set
	   :doset
	   :mapset
	   :set-add
	   :set-remove
	   :set-member
	   :copy-set))
