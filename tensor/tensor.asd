;;;; tensor.asd

(asdf:defsystem #:tensor
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:generic-math
               #:symbol-utils
	       #:list-utils
	       #:alexandria
               ;;debug
               #:macro-utils)
  :components ((:file "package")
	       (:file "tensor")))
