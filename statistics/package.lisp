(defpackage #:statistics
  (:use :cl)
  (:shadowing :mean)
  (:export :mean
           :standard-deviation
           :skew
           :kirtosis
           :moving-average))
