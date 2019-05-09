(require 'cl-ana)
(in-package :cl-ana)

;; Project settings. Change the path to something that works for you
(defproject resmacrotest
    "/home/ghollisjr/test/makeres-table/resmacrotest/"
  (list #'macrotrans #'tabletrans #'progresstrans)
  (fixed-cache 5))

(setf *print-progress* 100)

;; Source data
(defres npoints
  1000)
(defres bootstrap
  (loop
     for i below (res npoints)
     collecting
       (let* ((x (shaped-random #'normal-cdf-inv))
              (y (expt x 2)))
         (list :x x
               :y y))))
(defres data
  (srctab (plist-opener (res bootstrap))))

;; Test the nrows, mean, and standard deviation res-macros
(defres (data nrows)
  (dotab-nrows (res data)))
(defres (bootstrap nrows)
  (length (res bootstrap)))

(defres (data x mean)
  (dotab-mean (res data)
              (field x)))
(defres (bootstrap x mean)
  (mean (mapcar (lambda (x)
                  (getf x :x))
                (res bootstrap))))

(defres (data x standard-deviation)
  (dotab-standard-deviation (res data)
                            (field x)))
(defres (bootstrap x standard-deviation)
  (standard-deviation (mapcar (lambda (x)
                                (getf x :x))
                              (res bootstrap))))

;; Try something weird:
(defres (data (* x y) standard-deviation)
  (dotab-standard-deviation (res data)
                            (* (field x)
                               (field y))))

;; Should be equal to:
(defres (data (expt x 3) standard-deviation)
  (dotab-standard-deviation (res data)
                            (expt (field x) 3)))
