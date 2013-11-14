;;;; hdf-typespec.asd

(asdf:defsystem #:hdf-typespec
  :serial t
  :description "Utilities for creating/reading HDF5 types from/into
  typespecs."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils
	       #:string-utils
	       #:memoization
	       #:cffi
	       #:typespec
	       #:alexandria)
  :components ((:file "package")
	       (:file "hdf-typespec")))
