(in-package :math-functions)

(defgeneric erf (x)
  (:documentation "the error function")
  (:method ((x number))
    (gsll:erf (->double-float x))))

(defgeneric normal-pdf (x)
  (:documentation "Standard normal probability density function")
  (:method ((x number))
    (/ (exp (/ (- (* x x))
               2))
       (sqrt (* 2
                pi)))))

(defgeneric normal-cdf (x)
  (:documentation "Standard normal cumulative density function")
  (:method ((x number))
    (gsll:gaussian-p (->double-float x) 1d0)))

(defgeneric normal-cdf-inv (x)
  (:documentation "Inverse of normal CDF")
  (:method ((x number))
    (gsll:gaussian-pinv (->double-float x) 1d0)))
