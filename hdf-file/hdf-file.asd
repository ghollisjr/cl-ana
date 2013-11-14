;;;; hdf-file.asd

(asdf:defsystem #:hdf-file
  :serial t
  :description "HDF5 file access functions & macros"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:cffi
	       #:hdf-cffi)
  :components ((:file "package")
	       (:file "hdf-file")))
