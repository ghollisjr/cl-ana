(in-package :cl-ana.math-functions)

(defun logistic (x)
  "Logistic sigmoid function"
  (expt (+ 1 (exp (- x))) -1))

(defun logistic-derivative (x)
  "Derivative of logistic sigmoid function"
  (let ((f (logistic x)))
    (* f (- 1 f))))
