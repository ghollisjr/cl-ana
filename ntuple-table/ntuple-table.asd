;;;; ntuple-table.asd

(asdf:defsystem #:ntuple-table
  :serial t
  :description "Table subclass specializing on ntuples as implemented via GSLL"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils
	       #:cffi
	       #:gsll
	       #:gsl-cffi
	       #:table
               #:typed-table
	       #:typespec
	       #:alexandria)
  :components ((:file "package")
	       (:file "ntuple-table")))
