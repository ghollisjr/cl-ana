(defpackage #:makeres-graphviz
  (:use :cl
        :external-program
        :makeres)
  (:export
   ;; project graph -> dot code:
   :dot-compile
   :dot->ps
   :dot->png))
