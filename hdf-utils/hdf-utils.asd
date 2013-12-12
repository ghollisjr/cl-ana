;;;; hdf-utils.asd

(asdf:defsystem #:hdf-utils
  :serial t
  :description "Utilities for more easily using the HDF5 CFFI
  functions"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:cffi
               #:hdf-cffi
               #:alexandria)
  :components ((:file "package")
	       (:file "hdf-utils")))
