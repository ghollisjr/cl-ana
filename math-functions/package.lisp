(defpackage :math-functions
  (:use :cl)
  (:shadow :erf)
  (:export :erf
           :normal-pdf
           :normal-cdf
           :normal-cdf-inv))

(gmath:use-gmath :math-functions)
