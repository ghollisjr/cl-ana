;;;; fitting.asd

(asdf:defsystem #:fitting
  :serial t
  :description "Library for linear & non-linear least squares fitting
  which uses the MINPACK code as converted to LISP by f2cl."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:gsll
	       #:generic-math
	       #:error-propogation
	       #:alexandria)
  :components ((:file "package")
	       (:file "fitting")
	       (:file "functions")))
