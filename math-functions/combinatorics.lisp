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

(defmath factorial (x)
  (:documentation "Very naive factorial function")
  (:method ((x number))
    (labels ((rec (x &optional (result 1))
               (if (<= x 1)
                   result
                   (rec (1- x) (* x result)))))
      (rec x))))

(defmath npermutations (n r)
  (:documentation "Returns nPr, the number of permutations of n objects
taken r at a time without repetition.  Assumes reasonable values of n
and r.  Returns 1 for nP0.")
  (:method ((n number) (r number))
    (loop
       for k from (- n r) upto n
       for result = 1 then (* result k)
       finally (return result))))

(defmath ncombinations (n r)
  (:documentation "Returns nCr, the number of combinations of n
  objects taken r at a time without repetition.  Assumes reasonable
  values of n and r.")
  (:method ((n number) (r number))
    (cond
      ((zerop r)
       1)
      ((= n r)
       1)
      (t
       (let ((result 1))
         (do ((num n (1- num))
              (den 1 (1+ den)))
             ((> den r) result)
           (setf result (* result num))
           (setf result (truncate result den))))))))

(defun binomial (n r)
  "Nickname for ncombinations; returns the binomial coefficient of n
  and r."
  (ncombinations n r))

(defun multinomial (&rest ms)
  "Returns the multinomial coefficient where each element of ms is
taken to be the number of objects of a single type."
  (/ (factorial (sum ms))
     (product (mapcar #'factorial ms))))

;;;; Useful macros for working with combinatorics
;;; Loop over all nPr permutations.  E.g.,
;;;
;;; (for-permutations (indices 5 2) (print indices))
;;;
;;; would print out all of the possible 20 permutation index arrays.
;;;
;;; Note that for nP0, a single iteration of body is evaluated with an
;;; empty index array.
(defmacro for-permutations ((var n r) &body body)
  "Iterates over all permutations of r objects taken from n total,
binding an array of r index values to var and evaluating body with
that binding.  If you want to actually permute objects in a
list/array/sequence, use the bound index array to find permutations of
those objects."
  (alexandria:with-gensyms (npr nn rr i j norder x k)
    `(let* ((,nn ,n)
            (,rr ,r)
            (,npr (npermutations ,nn ,rr))
            (,var (make-array ,rr :element-type 'integer :initial-element 0)))
       (loop
          for ,i below ,npr
          do
            (loop
               for ,j below ,rr
               do
                 (let* ((,norder (- ,nn ,j))
                        (,x (mod ,i ,norder)))
                   (setf (aref ,var ,j)
                         ,x)
                   (loop
                      for ,k below ,j
                      when (>= ,x
                               (aref ,var ,k))
                      do (incf (aref ,var ,j)))))
            (progn
              ,@body)))))
