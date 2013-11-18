;;;; gmath-lisp-matrix

(in-package :gmath-lisp-matrix)

;;;; Integrating the lisp-matrix vectors and matrices into the generic
;;;; math framework.

;;; Vector-specific functionality:

(defmethod add ((v1 vector-like) (v2 vector-like))
  (v+ v1
      v2))

(defmethod sub ((v1 vector-like) (v2 vector-like))
  (v- v1
      v2))

;;; Matrix functionality:

(defmethod add ((m1 matrix-like) (m2 matrix-like))
  (m+ m1 m2))

(defmethod sub ((m1 matrix-like) (m2 matrix-like))
  (m- m1 m2))

(defmethod-commutative mult (x (m matrix-like))
  (scal (float x 0d0)
	m))

(defmethod mult ((x matrix-like) (y matrix-like))
  (m* x y))

(defmethod div ((m matrix-like) x)
  (scal (unary-div (float x 0d0))
	m))
