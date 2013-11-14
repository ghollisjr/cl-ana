;;;; list-utils.asd

(asdf:defsystem #:list-utils
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:functional-utils)
  :components ((:file "package")
	       (:file "list-utils")))
