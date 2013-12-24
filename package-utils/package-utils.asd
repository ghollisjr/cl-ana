(asdf:defsystem #:package-utils
  :serial t
  :author "Gary Hollis"
  :license ""
  :description "Provides various utilities for using packages"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "package-utils")))
