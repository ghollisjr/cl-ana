(asdf:defsystem #:serialization
  :serial t
  :description "serialization provides functionality for writing
  various objects to HDF5 files as datasets (which is the only way
  with HDF5)."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:histogram
               #:hdf-utils
               #:hdf-table)
  :components ((:file "package")
               (:file "histogram")))
