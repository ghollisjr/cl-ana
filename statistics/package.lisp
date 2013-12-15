(defpackage #:statistics
  (:use :cl)
  (:shadow :mean
           :standard-deviation)
  (:export :mean
           :mean-accumulator
           :standard-deviation
           :skewness
           :kirtosis
           :moving-average))

(gmath:use-gmath :statistics)
