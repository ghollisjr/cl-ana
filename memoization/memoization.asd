;;;; memoization.asd

(asdf:defsystem #:memoization
  :serial t
  :description "Provides memoized functions"
  :author "Gary Hollis"
  :license ""
  :depends-on ()
  :components ((:file "package")
	       (:file "memoization")))
