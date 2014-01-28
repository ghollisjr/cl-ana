(defsystem #:gnuplot-interface
  :serial t
  :author "Gary Hollis"
  :license "GPLv3"
  :description "A simple interface to gnuplot with session spawning
  and messages passed directly to the gnuplot session(s)."
  :depends-on (#:external-program)
  :components ((:file "package")
               (:file "gnuplot-interface")))
