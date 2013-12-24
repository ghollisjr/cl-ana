;;;; package.lisp

(defpackage #:tensor 
  (:use #:cl
	#:list-utils
        #:symbol-utils
        #:alexandria
        ;;debug
        #:macro-utils)
  (:export :make-tensor
	   :tensor-ref
           :tensor-flat-ref ; for flattened-index reference
	   :tensor-map
	   :tensor-+
	   :tensor--
	   :tensor-*
	   :tensor-/
	   :tensor-rank
	   :tensor-dimensions
	   :sequencep
	   :tensor-contract))

(gmath:use-gmath :tensor)
