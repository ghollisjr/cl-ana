(defsystem #:makeres
  :serial t
  :author "Gary Hollis"
  :description "makeres is a make-like tool for building analysis
  results in Common Lisp"
  :license "GPLv3"
  :depends-on (#:cl-ana)
  :components ((:file "package")
               (:file "hash-graph")
               (:file "makeres")))
