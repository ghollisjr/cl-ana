;;;; memoization.asd

(asdf:defsystem #:memoization
  :serial t
  :description "Provides memoized functions"
  :author "Gary Hollis"
  :license ""
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "memoization")))
