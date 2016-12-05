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

(require 'cl-ana)

(in-package :cl-ana)

(defproject tabletrans-test
    "/home/ghollisjr/test/makeres-table/tabletrans-test"
  (list #'macrotrans #'tabletrans #'progresstrans)
  (fixed-cache 5))

;; (setf *print-progress* t)

(defres source
  (srctab (plist-opener '((:x 1)
                          (:x 2)
                          (:x 3)))))

(defres filtered
  (ltab (res source)
      ()
    (when (< (field x) 4)
      ;; you only have to add new fields, all source
      ;; fields not shadowed are still available:
      (push-fields
       ;; new field y, x is still accessible, unshadowed
       (y (* 2 (field x)))))))

(defres (filtered sum)
  (dotab (res filtered)
      ((sum 0))
      sum
    (incf sum (field y))))

(defres filtered2
  (ltab (res source)
      ()
    (when (< (field x) 5)
      (push-fields
       ;; shadow field x:
       (x (sqrt (field x)))
       ;; new field y:
       (y (field x))))))

(defres canon
  (tab (res filtered)
      ()
      (hdf-opener "/home/ghollisjr/canon.h5"
                  '(("X" . :int)
                    ("Y" . :float)
                    ("Z" . :float)))
    (push-fields (x (field x))
                 (y (sqrt (field y)))
                 (z (float
                     (expt (field y)
                           2))))))

(defres (canon (sum x))
  (dotab (res canon)
      ((sum 0))
      sum
    (incf sum (field x))))

(defres (canon count)
  (dotab (res canon)
      ((count 0))
      count
    (incf count)))

(defres (canon (mean x))
  (/ (res (canon (sum x)))
     (res (canon count))))

(defres (filter canon)
  (ltab (res canon)
      ()
    (when (< (field x)
             (res (canon (sum x))))
      (push-fields
       (x (field x))))))

(defres (filter source)
  (ltab (res source)
      ()
    (when (< (field x)
             (res (canon (sum x))))
      (push-fields
       (x (field x))))))

(defres other
  (tab (res filtered2)
      ()
      (hdf-opener "/home/ghollisjr/other.h5"
                  '(("X" . :int)))
    (push-fields
     (x (field y)))))

(defres (canon (variance x))
  (dotab (res canon)
      ((sum 0)
       (count 0))
      (/ sum
         (- count 1))
    (incf sum
          (expt (- (field x)
                   (res (canon (mean x))))
                2))))

;; Pass deconstruction should be:
;;
;; source:
;;
;; 1. canon, other, (canon count), (canon (sum x))
;;
;; canon:
;;
;; 1. (canon (variance x))
