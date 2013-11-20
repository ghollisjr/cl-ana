;;;; tensor.asd

(asdf:defsystem #:tensor
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:generic-math
	       #:list-utils
	       #:alexandria)
  :components ((:file "package")
	       (:file "tensor")))
