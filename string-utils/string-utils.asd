;;;; string-utils.asd

(asdf:defsystem #:string-utils
  :serial t
  :description "String utilities not already provided"
  :author "Gary Hollis"
  :license ""
  :depends-on ()
  :components ((:file "package")
	       (:file "string-utils")))
