(defpackage #:statistics
  (:use :cl)
  (:shadow :mean)
  (:export :mean
           :mean-accumulator
           :standard-deviation
           :skew
           :kirtosis
           :moving-average))

(gmath:use-gmath :statistics)
