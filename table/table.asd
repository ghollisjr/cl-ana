;;;; table.asd

(asdf:defsystem #:table
  :serial t
  :description "Table class, an abstraction of a list of rows (which
  are lists)"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:macro-utils
	       #:list-utils
	       #:string-utils
	       #:functional-utils
	       #:alexandria)
  :components ((:file "package")
	       (:file "table")
	       (:file "table-chain")
	       (:file "plist-table")))
