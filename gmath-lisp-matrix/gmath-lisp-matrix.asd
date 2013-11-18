;;;; gmath-lisp-matrix.asd

(asdf:defsystem #:gmath-lisp-matrix
  :serial t
  :description "Provides generic math interface to the lisp-matrix
  library."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:generic-math
	       #:lisp-matrix
	       #:lisp-matrix-utils)
  :components ((:file "package")
	       (:file "gmath-lisp-matrix")))
