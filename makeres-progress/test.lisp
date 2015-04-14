(require 'cl-ana)

(in-package :cl-ana)

(in-project makeres-progress-test)

(settrans (list #'progresstrans) :op :set)

(defres a
  (list 1 2 3))

(defres b
  (list 2 3 4))

(defres c
  (+ (res a)
     (res b)))
