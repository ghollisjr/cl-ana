;;;; set.asd

(asdf:defsystem #:set
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:functional-utils)
  :components ((:file "package")
	       (:file "set")))
