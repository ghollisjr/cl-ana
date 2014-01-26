;;;; csv-table.asd

(asdf:defsystem #:csv-table
  :serial t
  :description "Table subclass specializing on CSV files; allows for
  whatever lisp types are understood by the reader."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils
	       #:cl-csv
	       #:table
	       #:iterate
	       #:alexandria)
  :components ((:file "package")
	       (:file "csv-table")))
