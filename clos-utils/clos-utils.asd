(asdf:defsystem #:clos-utils
  :serial t
  :description "clos-utils provides various lacking utilities for
  working with structures and CLOS class instances as well as
  utilities for transforming them into clists which can be
  written/read with standard stream objects portably."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils
               #:symbol-utils)
  :components ((:file "package")
               (:file "clos-utils")))
