;;;; hdf-table.asd

(asdf:defsystem #:hdf-table
  :serial t
  :description "Table subclass specializing on hdf5 data"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils
	       #:hdf-cffi
	       #:hdf-file
	       #:table
	       #:rread-table
	       #:typespec
	       #:hdf-typespec
	       #:binary-tree
	       #:alexandria)
  :components ((:file "package")
	       (:file "hdf-table")
	       (:file "hdf-table-chain")))
