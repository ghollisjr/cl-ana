(require 'cl-ana)

(in-package :cl-ana)

(defproject makeres-test
    "/home/ghollisjr/cl-ana/makeres/makeres-test"
  (list #'macrotrans #'progresstrans)
  #'singleton-cache)

(defres a
  (list 1 2 3))

(defres (a sum)
  (sum (res a)))

(defres b
  5)

(defres (+ a b)
  (+ (res a)
     (res b)))

(defres craziness
  (range 1 2))

(defres z
  (+ (res a)
     (res b)
     (res (+ a b))))

(defres snapshot-test
  'a)

(defres fn
  (lambda ()
    (print t)))

;; Call to load-project and makeres to initialize project:

(load-project)
(makeres)
