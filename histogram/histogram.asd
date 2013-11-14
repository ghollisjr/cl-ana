;;;; histogram.asd

(asdf:defsystem #:histogram
  :serial t
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils
	       #:iterate
	       #:generic-math
	       #:tensor
	       #:alexandria
	       #:fitting)
  :components ((:file "package")
	       (:file "uniform-binning")
	       (:file "histogram")
	       (:file "contiguous-histogram")
	       (:file "sparse-histogram")))
