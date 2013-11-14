;;;; binary-tree.asd

(asdf:defsystem #:binary-tree
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils)
  :components ((:file "package")
	       (:file "binary-tree")))
