;;;; error-propogation.asd

(asdf:defsystem #:error-propogation
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:generic-math)
  :components ((:file "package")
	       (:file "error-propogation")))
