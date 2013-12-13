;;;; reusable-table.asd

(asdf:defsystem #:reusable-table
  :serial t
  :description "Table wrapper for reading which automatically re-loads
  table for reading after fully reading the contents (not guarranteed
  by the raw table types)."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:table
	       #:alexandria)
  :components ((:file "package")
	       (:file "reusable-table")))
