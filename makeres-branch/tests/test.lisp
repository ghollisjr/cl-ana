(require 'cl-ana)

(in-package :cl-ana)

(defproject branch
    "/home/ghollisjr/test/makeres-branch/"
  (list #'branchtrans)
  (fixed-cache 5))

(defparameter *xs*
  (list 1 2 3 4 5))

(defparameter *ys*
  (list 6 7 8 9 10))

(defres double-x
  (branch *xs*
    (* (branch) 2)))

(defres double-y
  (branch *ys*
    (* (branch) 2)))

(defres exp-double-x
  (branch (res double-x)
    (exp (res double-x))))

(defres sin-exp-double-x
  (branch (res exp-double-x)
    (sin (res exp-double-x))))

(defres exp-double-y
  (branch (res double-y)
    (exp (res double-y))))

;; double branching:
(defres nils
  (branch *xs*
    (branch *ys*
      nil)))
