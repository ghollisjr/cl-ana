(require 'cl-ana)

(in-package :cl-ana)

(in-project makeres-block-test)

(settrans (list #'blocktrans) :op :set)

(defres a
  (list 1 2 3))

(defresblock (b c d)
  (setres b (sum (res a)))
  (setres c (zip (res a) (list (resfn 'b))))
  (setres d 5))
