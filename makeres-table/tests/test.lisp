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

;; NOTE: tabletrans is an example of a graph transformation which
;; needs to have makeres run even when the only thing updated is the
;; input parameters.

;; Demo of tabletrans

(in-project tabletrans-simple-test)

(settrans (list #'macrotrans #'tabletrans))

(defpars
    ((nrows 5)))

;; source table
(defres table
  (wrap-for-reuse
   (open-plist-table
    (mapcar (lambda (x)
              (list :x x))
            (loop
               for i below (par nrows)
               collecting i)))))

(deflfields table
    ((y (* 2 (field x)))))

(defres test
  (dotab (res table)
      ()
      nil
    (let ((z (sqrt (field x))))
      (print (field z)))))
    ;;(print 'test)))

;; average:
(defres mean
  (dotab (res table)
      ((sum 0)
       (count 0))
      (progn
        (print 'return-mean)
        (the float (float (/ sum count))))
    (print 'loop-mean)
    (incf sum (field x))
    (incf count)))

(defres ymean
  (dotab (res table)
      ((sum 0)
       (count 0))
      (/ sum count)
    (print 'ymean)
    (incf sum (field y))
    (incf count)))

(defres max
  (dotab (res table)
      ((max nil))
      max
    (if (not max)
        (setf max (field x))
        (when (> (field x) max)
          (setf max (field x))))))

(defres min
  (dotab (res table)
      ((min nil))
      min
    (if (not min)
        (setf min (field x))
        (when (< (field x) min)
          (setf min (field x))))))

(defres variance
  (dotab (res table)
      ((sum-squares 0)
       (count 0)) ; safe since variance happens in second pass
      (the float
           (float (/ sum-squares
                     (1- count))))
    (incf sum-squares
          (expt (- (field x)
                   (res mean))
                2))
    (incf count)))

(defres sigma
  (sqrt (res variance)))
