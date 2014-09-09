(defpackage #:makeres-graphviz
  (:use :cl
        :external-program
        :makeres)
  (:export
   ;; project graph -> dot code:
   :dot-compile
   :dot->ps
   :dot->png))

(package-utils:use-package-group :cl-ana :makeres-graphviz)
