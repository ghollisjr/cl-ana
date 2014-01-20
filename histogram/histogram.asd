;;;; histogram.asd

(asdf:defsystem #:histogram
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:clos-utils
               #:symbol-utils
               #:list-utils
               #:list-utils
	       #:macro-utils
	       #:iterate
	       #:generic-math
	       #:map
	       #:tensor
	       #:alexandria
	       #:fitting)
  :components ((:file "package")
	       (:file "histogram")
	       (:file "rectangular-histogram")
	       (:file "contiguous-histogram")
	       (:file "sparse-histogram")
               (:file "conversion")))
