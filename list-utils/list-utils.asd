;;;; list-utils.asd

(asdf:defsystem #:list-utils
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:functional-utils
               #:alexandria)
  :components ((:file "package")
	       (:file "list-utils")))
