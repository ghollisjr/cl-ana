;;;; package.lisp

(defpackage :list-utils
  (:use :cl
	:functional-utils)
  (:export :range
	   :singletonp
	   :zip
	   :unzip
	   :transpose
	   :cartesian-product
	   :every-nth
	   :except-nth
	   :except-at
	   :compress
	   :list-less-than
	   :list-greater-than
	   :aref-by-list))
