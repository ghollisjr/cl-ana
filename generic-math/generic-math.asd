;;;; generic-math.asd

(asdf:defsystem #:generic-math
  :serial t
  :description "Provides a few generic math functions.  Not sure why
  Common Lisp doesn't do this from the start, but I'm doing it
  anyways."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:package-utils
               #:list-utils)
  :components ((:file "package")
	       (:file "generic-math")
	       (:file "number")))
