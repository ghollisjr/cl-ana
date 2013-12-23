(in-package :math-functions)

(defmath ->double-float (x)
  (:documentation "Converts numerical object into a double-float form;
  does not need to be an actual double-float result, but where
  appropriate constituents are converted into double-float
  values.")
  (:method ((x real))
    (float x 0d0))
  (:method ((x complex))
    (complex (float (realpart x) 0d0)
             (float (imagpart x) 0d0))))
