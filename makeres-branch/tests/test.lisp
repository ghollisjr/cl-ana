(require 'cl-ana)

(in-package :cl-ana.makeres-branch)

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
