;;;; typespec.asd

(asdf:defsystem #:typespec
  :serial t
  :description "Simple library for creating CFFI cstructs from a
  generic type specification, the typespec."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:list-utils
	       #:string-utils
	       #:memoization
	       #:cffi)
  :components ((:file "package")
	       (:file "typespec")))
