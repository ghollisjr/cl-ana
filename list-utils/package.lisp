;;;; package.lisp

(defpackage :list-utils
  (:use :cl
	:functional-utils)
  (:export :range
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
	   :aref-by-list
	   :make-offsets
           ;; Paul Graham's stuff
	   :single
           :append1
           :conc1
           :mklist
           :longer
           :group
           :prune
           :find2
           :before
           :after
           :duplicate))
