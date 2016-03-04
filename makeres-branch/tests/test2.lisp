(require 'cl-ana)

(in-package :cl-ana)

(defproject branch
    "/home/ghollisjr/test/makeres-branch/"
  (list #'branchtrans)
  (fixed-cache 5))

(defres (branching xs)
  (branch (list 1 2 3 4 5)))

(defres (branching ys)
  (branch (list 6 7 8 9 10)))

(defres double-x
  (branch (res (branching xs))
          (* (branch) 2)))

(defres double-y
  (branch (res (branching ys))
          (* (branch) 2)))

(defres exp-double-x
  (branch (res (branching xs))
          (exp (res double-x))))

(defres sin-exp-double-x
  (branch (res (branching xs))
          (sin (res exp-double-x))))

(defres exp-double-y
  (branch (res (branching ys))
          (exp (res double-y))))

;; double branching:
(defres nils
  (branch (res (branching xs))
          (branch (res (branching ys))
                  nil)))
