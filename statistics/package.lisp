(defpackage #:statistics 
  (:use :cl
        :list-utils)
  (:shadow :mean
           :standard-deviation)
  (:export :mean
           :mean-accumulator
           :standard-deviation
           :skewness
           :kirtosis
           :moving-average
           :quantiles
           :percentiles
           :probability-plot))

(gmath:use-gmath :statistics)
