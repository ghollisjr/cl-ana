(in-package :cl-ana.math-functions)

(defun logistic (x)
  "Logistic sigmoid function"
  (expt (+ 1 (exp (- x))) -1))

(defun logistic-derivative (x)
  "Derivative of logistic sigmoid function"
  (let ((f (logistic x)))
    (* f (- 1 f))))

(defmath sinc (x)
  (:documentation "Cardinal sine function (not normalized)")
  (:method ((x number))
    (protected-div (sin x) x :protected-value (coerce 1 (type-of x))))
  (:method ((x double-float))
    (protected-div (sin x) x :protected-value 1d0))
  (:method ((x float))
    (protected-div (sin x) x :protected-value 1f0)))

(defmath sincn (x)
  (:documentation "Cardinal sine function (normalized)")
  (:method ((x number))
    (let ((xx (* pi x)))
      (protected-div (sin xx) xx :protected-value (coerce 1 (type-of x)))))
  (:method ((x double-float))
    (let ((xx (* pi x)))
      (protected-div (sin xx) xx :protected-value 1d0)))
  (:method ((x float))
    (let ((xx (* (float pi 1f0) x)))
      (protected-div (sin xx) xx :protected-value 1f0))))
