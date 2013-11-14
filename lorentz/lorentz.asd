;;;; lorentz.asd

(asdf:defsystem #:lorentz
  :serial t
  :description "Implements lorentz vectors, lorentz transformations,
  etc., built on top of the lisp-matrix library."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:generic-math
	       #:lisp-matrix
	       #:xarray
	       #:iterate)
  :components ((:file "package")
	       (:file "lorentz")))
