(require 'cl-ana)

(in-package :cl-ana)

(defproject makeres-block-test
    "/home/ghollisjr/test/makeres-block/test"
  (list #'blocktrans)
  (fixed-cache 5))

(defres a
  (list 1 2 3))

(defresblock (b c d)
  (setres b (sum (res a)))
  (setres c (zip (res a) (list (resfn 'b))))
  (setres d 5))
