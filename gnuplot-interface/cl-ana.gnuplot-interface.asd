(defsystem #:cl-ana.gnuplot-interface
  :serial t
  :author "Gary Hollis"
  :description "Interface for spawning & communicating with gnuplot
  sessions from LISP."
  :license "GPLv3"
  :depends-on (#:external-program)
  :components ((:file "package")
               (:file "gnuplot-interface")))
