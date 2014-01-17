;;;; typespec.asd

(asdf:defsystem #:typespec
  :serial t
  :description "Simple library for creating CFFI cstructs from a
  generic type specification, the typespec."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:int-char
               #:list-utils
	       #:string-utils
               #:symbol-utils
               #:tensor
	       #:memoization
               #:alexandria
	       #:cffi)
  :components ((:file "package")
	       (:file "typespec")))
