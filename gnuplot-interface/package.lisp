(defpackage #:cl-ana.gnuplot-interface
  (:use :cl
        :external-program)
  (:export :gnuplot-init
           :gnuplot-close
           :gnuplot-cmd))
