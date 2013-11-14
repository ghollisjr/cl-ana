;;;; rread-table.asd

(asdf:defsystem #:rread-table
  :serial t
  :description "Interface for using tables which allow random reading."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:table)
  :components ((:file "package")
	       (:file "rread-table")))
