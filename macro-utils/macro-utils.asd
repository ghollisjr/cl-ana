;;;; macro-utils.lisp
(asdf:defsystem #:macro-utils
  :serial t
  :description "Basic macro utilities like with-gensyms"
  :author "Gary Hollis"
  :license ""
  :depends-on ()
  :components ((:file "package")
	       (:file "macro-utils")))
