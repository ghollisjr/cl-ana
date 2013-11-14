;;;; hdf-cffi.asd

(asdf:defsystem #:hdf-cffi
  :serial t
  :description "HDF5 cffi bindings"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:cffi)
  :components ((:file "package")
	       (:file "hdf-cffi")))
