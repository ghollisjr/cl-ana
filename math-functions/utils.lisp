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

(in-package :cl-ana.math-functions)

(defmath ->double-float (x)
  (:documentation "Converts numerical object into a double-float form;
  does not need to be an actual double-float result, but where
  appropriate constituents are converted into double-float
  values.")
  (:method ((x real))
    (float x 0d0))
  (:method ((x complex))
    (complex (float (realpart x) 0d0)
             (float (imagpart x) 0d0))))

(defun periodic-shift (x lo hi)
  "Shifts x into the range [lo, hi] through interval multiples of (-
hi lo).  Useful for shifting angles into a different range or working
with periodic quantities in general."
  (let* ((range (- hi lo))
         (k (ceiling (/ (- lo x) range))))
    (+ x (* k range))))

;;;; Solving 1-D problems with interval bisection
(defun solve-interval-bisection (fn interval
                                 &key
                                   (value 0d0)
                                   (prec 1d-4))
  (flet ((exists (x y)
           (minusp (* (signum (- (funcall fn x) value))
                      (signum (- (funcall fn y) value)))))
         (middle (x y)
           (* 0.5d0 (+ x y))))
    (let* ((xlo (car interval))
           (xhi (cdr interval))
           (diff (- xhi xlo)))
      (cond
        ((zerop (- (funcall fn xlo) value))
         xlo)
        ((zerop (- (funcall fn xhi) value))
         xhi)
        (t
         (when (exists xlo xhi)
           (let* ((iter 0))
             (loop
               while (> diff prec)
               do (let* ((xm (middle xlo xhi)))
                    (incf iter)
                    (if (zerop (- (funcall fn xm) value))
                        (return-from solve-interval-bisection
                          xm)
                        (if (exists xlo xm)
                            (setf xhi xm)
                            (setf xlo xm)))
                    (setf diff (- xhi xlo))))
             (values (middle xlo xhi)
                     iter))))))))

;; Ensure that integer logic results are positive integers
(defun logpos (x nbits)
  "Returns an integer representing bit-logic in nbits of space as
  positive instead of negative.

E.g.: (logeqv #b111 #b111) ==> -1
      (logpos (logeqv #b111 #b111) 3) ==> 7"
  (logand x (1- (ash 1 nbits))))

;; Count leading zeros
(defun clz (x)
  "Count leading zeroes function"
  (declare (integer x))
  (1-
   (integer-length
    (logand x
            (lognot (1- x))))))
