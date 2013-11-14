;;;; gsl-cffi.asd

(asdf:defsystem #:gsl-cffi
  :serial t
  :description "Functions for interfacing with GSL which don't seem to
  work/aren't currently provided through GSLL."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:cffi)
  :components ((:file "package")
	       (:file "gsl-cffi")))
