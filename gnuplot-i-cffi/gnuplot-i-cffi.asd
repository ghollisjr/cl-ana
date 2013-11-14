;;;; gnuplot-i-cffi.asd

(asdf:defsystem #:gnuplot-i-cffi
  :serial t
  :description "gnuplot_i CFFI minimal interface"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:cffi)
  :components ((:file "package")
	       (:file "gnuplot-i-cffi")))
