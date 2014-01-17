(asdf:defsystem #:int-char
  :serial t
  :description "This is the result of either the lunacy of the
  committee in charge of Common Lisp's libraries in which they removed
  the int-char function or the laziness of SBCL's implementers in not
  providing it.  For shame..."
  :author "Gary Hollis"
  :depends-on ()
  :components ((:file "package")
               (:file "int-char")))
