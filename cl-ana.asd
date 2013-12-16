(defsystem #:cl-ana
  :serial t
  :author "Gary Hollis"
  :description "Convenient system which loads all components for
  analysis I've been working on so far."
  :license ""
  :depends-on (#:generic-math
               #:tensor
               #:table
               #:hdf-table
               #:ntuple-table
               #:csv-table
               #:reusable-table
               #:linear-algebra
               #:lorentz
               #:histogram
               #:error-propogation
               #:fitting
               #:file-utils
               #:math-functions
               #:statistics
               #:plotting)
  :components ((:file "package")))
