;;;; fitting.asd

(asdf:defsystem #:fitting
  :serial t
  :description "Library for linear & non-linear least squares fitting;
  makes use of GSLL's fit functions."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:gsll
	       #:generic-math
	       #:error-propogation
	       #:map
	       #:alexandria)
  :components ((:file "package")
	       (:file "fitting")
	       (:file "functions")))
