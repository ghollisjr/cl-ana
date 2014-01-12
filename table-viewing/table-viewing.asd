;;;; table.asd

(asdf:defsystem #:table-viewing
  :serial t
  :description "Library for easily viewing the contents of a
  table (preferably a reusable table)"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:alexandria
               #:string-utils
               #:table
               #:histogram
               #:plotting
               #:generic-math)
  :components ((:file "package")
	       (:file "table-viewing")))
