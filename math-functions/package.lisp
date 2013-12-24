(defpackage :math-functions 
  (:use :cl)
  (:shadow :erf)
  (:export :erf
           :sinc
           :normal-pdf
           :normal-cdf
           :normal-cdf-inv
           ;; utils:
           :->double-float))

(gmath:use-gmath :math-functions)
