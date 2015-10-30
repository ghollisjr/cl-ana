;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
;;;;
;;;; This file is part of cl-ana.
;;;;
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.statistics)

(defgeneric mean (data)
  (:documentation "Returns mean (and count) of data")
  (:method (data)
    (let ((count (length data)))
      (values (/ (sum data) count)
              count)))
  (:method ((hist histogram))
    (let ((count (htint hist)))
      (values
       (/ (sum
           (mapcar (lambda (x)
                     (dbind (c x)
                            x
                            (* c x)))
                   (hbv hist)))
          count)
       count))))

(defgeneric variance (data)
  (:documentation "Returns variance of data")
  (:method (data)
    (multiple-value-bind (mean count)
        (mean data)
      (/ (sum (mapcar (lambda (x) (expt (- x mean) 2))
                      data))
         (- count 1))))
  (:method ((hist histogram))
    (multiple-value-bind (mean count)
        (mean hist)
      (/ (sum (mapcar (lambda (x)
                        (dbind (c x) x
                               (* c (expt (- x mean) 2))))
                      (hbv hist)))
         (- count 1)))))

(defun standard-deviation (data)
  (sqrt (variance data)))

(defun standard-scores (data)
  "Returns the list of standard-scores (number of standard deviations
away from the mean) for the data"
  (let ((mean (mean data))
        (sigma (standard-deviation data)))
    (mapcar (lambda (x)
              (/ (- x mean)
                 sigma))
            data)))

(defun sample-moment (data &key
                             (order 1)
                             (type :r))
  "Returns the order-th sample moment of the data according to type.

type can be :r (raw), :c (central), or :s (standardized).

second return value is size of data sample."
  (case type
    ;; raw
    (:r
     (mean (mapcar (lambda (x)
                     (expt x order))
                   data)))
    (:c
     (let ((mean (mean data)))
       (mean (mapcar (lambda (x)
                       (expt (- x mean)
                             order))
                     data))))
    (:s
     (let ((mean (mean data))
           (sigma (standard-deviation data)))
       (mean
        (mapcar (lambda (x)
                  (expt (/ (- x mean)
                           sigma)
                        order))
                data))))))

;; Technically, all the standard moments like mean, variance,
;; skewness, etc. should come from this k-statistic function, but
;; since I can't seem to find a reference which gives a formula for
;; all of them, I'm stuck with this approach where I basically splice
;; the function together.
(defun k-statistic (data order)
  "Returns the orderth k-statistic (unbiased estimator of orderth
cumulant)"
  (case order
    (1 (mean data))
    (2 (variance data))
    (3 (multiple-value-bind (m3 n)
           (sample-moment data
                          :order 3
                          :type :c)
         (* n n
            (/
             (* (- n 1)
                (- n 2)))
            m3)))
    (4 (multiple-value-bind (m4 n)
           (sample-moment data
                          :order 4
                          :type :c)
         (let ((m2 (sample-moment data
                                  :order 2
                                  :type :c)))
           (/ (* n n
                 (- (* (+ n 1)
                       m4)
                    (* 3
                       (- n 1)
                       m2
                       m2)))
              (* (- n 1)
                 (- n 2)
                 (- n 3))))))))

(defun skewness (data)
  (k-statistic data 3))

(defun kirtosis (data)
  (k-statistic data 4))

(defun quantiles (data)
  "Returns an alist mapping each datum to its quantile."
  (let ((denom (+ 1 (length data)))
        (compressed
         (compress data
                   :test #'equal
                   :singleton-pairs t
                   :sort-by #'<))
        (acc 0d0))
    (loop
       for (x . c) in compressed
       do (incf acc c)
       collecting (cons x (/ acc denom)) into result
       finally (return result))))

(defun qq-plot (data cdf-inv)
  "Returns data suitable for a Q-Q probability scatter plot given data
and inverse of cummulative density function.  Useful for checking
whether data is distributed according to cdf or not."
  (mapcar (lambda (x)
            (cons (funcall cdf-inv (cdr x))
                  (car x)))
          (quantiles data)))

(defun percentiles (data)
  "Returns an alist mapping each datum to its percentile."
  (let ((denom (length data))
        (compressed
         (compress (sort (copy-list data) #'<)
                   :test #'equal :singleton-pairs t))
        (acc 0d0))
    (loop
       for (x . c) in compressed
       do (incf acc c)
       collecting (cons x (/ acc denom)) into result
       finally (return result))))

(defun mean-accumulator (&rest sample)
  "Utility function used by moving-average.  Returns two values:

1. A function which returns the (updated) moving/running average each
time you call it on a value.  The samples you give provide the
initilization data and the size of the data subset to maintain while
computing the moving average.

2. The initial value for the moving average; this is so that
subsequent calls on the moving average function will return the
immediate updated mean."
  (let* ((lst (copy-list sample))
         (length (length lst))
         (end (last lst))
         (initial-mean (/ (sum lst)
                          length)))
    (values (lambda (x)
              (setf (cdr end)
                    (list x))
              (setf end
                    (cdr end))
              (setf lst
                    (rest lst))
              (/ (sum lst) length))
            initial-mean)))

(defun moving-average (data subset-length)
  "Returns a list of the moving averages on the data (must be a list)
with subsets of length subset-length.  subset-length is not checked to
be in bounds, so be careful."
  (multiple-value-bind (acc init-mean)
      (apply #'mean-accumulator (subseq data 0 subset-length))
    (cons init-mean
          (mapcar acc
                  (subseq data subset-length)))))

(defun determination-coefficient (data function)
  "Calculates R^2, or the coefficient of determination, for the
function against the data.  data should have type with map->alist
defined."
  (let ((alist (map->alist data)))
    (- 1
       (/ (sum (expt (mapcar (lambda (cons)
                               (- (funcall function (car cons))
                                  (cdr cons)))
                             alist)
                     2))
          (let* ((ys (mapcar #'cdr alist))
                 (mean (mean ys)))
            (sum (expt (mapcar (lambda (x)
                                 (- x mean))
                               ys)
                       2)))))))
