;;;; typed-table.asd

(asdf:defsystem #:typed-table
  :serial t
  :description "Table types which have typed columns"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils
               #:string-utils
               #:symbol-utils
	       #:table
	       #:typespec
	       #:alexandria)
  :components ((:file "package")
	       (:file "typed-table")))
