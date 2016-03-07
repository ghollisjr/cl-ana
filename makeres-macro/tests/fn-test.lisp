(require 'cl-ana)

(in-package :cl-ana)

(defproject fn-test "/home/ghollisjr/test/fn-test"
  (list #'macrotrans #'progresstrans)
  (fixed-cache 5))

(defres x
  2)

(define-res-function *x (x)
  (* (res x) x))

(defres y
  (*x 5))

(define-res-function fn (a b &rest keys &key c)
  (list :a a :b b :keys keys :c c :x (res x)))

(defres z
  (fn (res x)
      (res y)
      :c (* (res x) 2)))
