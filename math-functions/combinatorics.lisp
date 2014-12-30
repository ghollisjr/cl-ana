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
  (:documentation"Returns nPr, the number of permutations of n objects
taken r at a time without repetition.")
  (:method ((n number) (r number))
    (/ (factorial n)
       (factorial (- n r)))))

(defmath ncombinations (n r)
  (:documentation "Returns nCr, the number of combinations of n
  objects taken r at a time without repetition.")
  (:method ((n number) (r number))
    (/ (npermutations n r)
       (factorial r))))

(defun binomial (n r)
  "Nickname for ncombinations; returns the binomial coefficient of n
  and r."
  (ncombinations n r))

(defun multinomial (&rest ms)
  "Returns the multinomial coefficient where each element of ms is
taken to be the number of objects of a single type."
  (/ (factorial (sum ms))
     (product (mapcar #'factorial ms))))
