(defpackage :cl-ana.spline
  (:use :cl
        :cl-ana.fitting
        :cl-ana.macro-utils
        :cl-ana.list-utils
        :cl-ana.tensor
        :cl-ana.math-functions)
  (:export
   :polynomial-spline ; supports polynomial splines of arbitrary order
   ;; struct accessors
   :polynomial-spline-degree
   :polynomial-spline-coefs
   :polynomial-spline-xs
   :polynomial-spline-deltas
   :gsl-spline ; supports splines provided by GSL via GSLL
   :polynomial-spline-constraint ; generate consraint equations
   :evaluate-polynomial-spline
   :evaluate-polynomial-spline-derivative ; derivatives to any degree
   :evaluate-polynomial-spline-integral ; definite integral
   
   ))

(cl-ana.gmath:use-gmath :cl-ana.spline)
