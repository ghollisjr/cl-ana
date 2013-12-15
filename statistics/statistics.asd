;;;; statistics.asd

(asdf:defsystem #:statistics
  :serial t
  :description "Table subclass specializing on hdf5 data"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:generic-math
               #:list-utils)
  :components ((:file "package")
	       (:file "statistics")))
