;;;; linear-algebra.asd

(asdf:defsystem #:linear-algebra
  :serial t
  :description "Linear algebra library (since lisp-matrix is too slow)
  which is integrated into the generic math framework."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:generic-math
	       #:tensor
	       #:list-utils)
  :components ((:file "package")
	       (:file "linear-algebra")))
