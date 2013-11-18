;;;; lisp-matrix-utils.asd

(asdf:defsystem #:lisp-matrix-utils
  :serial t
  :description "Provides utilities for working with lisp-matrix."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:lisp-matrix)
  :components ((:file "package")
	       (:file "lisp-matrix-utils")))
