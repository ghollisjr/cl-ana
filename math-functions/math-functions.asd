(asdf:defsystem :math-functions
  :serial t
  :author "Gary Hollis"
  :description "Auxiliary math functions provided within the
  generic-math framework."
  :license ""
  :depends-on (#:generic-math
               #:tensor
               #:gsll)
  :components ((:file "package")
               (:file "utils")
               (:file "distributions")))
