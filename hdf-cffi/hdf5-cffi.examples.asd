(defsystem hdf5-cffi.examples
  :serial t
  :description "hdf5-examples contains a collection of simple helper functions."
  :version "0.0.1"
  :author "Gerd Heber <gheber@hdfgroup.org>"
  :license "BSD"
  :depends-on (:hdf5-cffi)
  :components
  ((:module "examples"
            :components ((:file "common")))))
