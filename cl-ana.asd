(defsystem #:cl-ana
  :serial t
  :author "Gary Hollis"
  :description "Convenient system which loads all components for
  analysis I've been working on so far."
  :license ""
  :depends-on (#:package-utils
               #:generic-math
               #:math-functions
               ;; Make sure to place tensor after defining all gmath
               ;; generic functions
               #:tensor
               #:error-propogation
               #:table
               #:hdf-table
               #:ntuple-table
               #:csv-table
               #:reusable-table
               #:linear-algebra
               #:lorentz
               #:histogram
               #:fitting
               #:file-utils
               #:statistics
               #:plotting)
  :components ((:file "package")))
