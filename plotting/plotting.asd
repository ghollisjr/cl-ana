;;;; plotting.asd

(asdf:defsystem #:plotting
  :serial t
  :description "Table subclass specializing on hdf5 data"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:gnuplot-i-cffi
               #:map
               #:string-utils
               #:list-utils
               #:histogram)
  :components ((:file "package")
	       (:file "plotting")
               (:file "histogram-plotting")))
