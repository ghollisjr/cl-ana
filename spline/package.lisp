(defpackage :cl-ana.spline
  (:use :cl
        :cl-ana.fitting
        :cl-ana.macro-utils
        :cl-ana.list-utils
        :cl-ana.tensor
        :cl-ana.math-functions)
  (:export
   :natural-spline ; supports arbitrary order
   :gsl-spline ; supports splines provided by GSL via GSLL
   :evaluate-natural-spline
   :evaluate-natural-spline-derivative ; derivatives to any degree
   :evaluate-natural-spline-integral ; definite integral
   ))

(cl-ana.gmath:use-gmath :cl-ana.spline)
