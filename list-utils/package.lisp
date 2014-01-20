;;;; package.lisp

(defpackage #:list-utils 
  (:use :cl
	:functional-utils
        :alexandria)
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
           ;; Useful looping macro over plists:
           :do-plist
           ;; Paul Graham's stuff (and some of my improvements)
           :length-equal
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
           :duplicate
           :permute))
