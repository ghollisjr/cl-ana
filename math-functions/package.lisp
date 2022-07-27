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

(defpackage :cl-ana.math-functions 
  (:use :cl)
  (:shadow :erf)
  (:shadow :factorial)
  (:export
   ;; distributions:
   :erf
   :sinc
   :normal-pdf
   :normal-cdf
   :normal-cdf-inv
   :uniform-cdf
   :uniform-cdf-inv
   ;; combinatorics:
   :factorial
   :npermutations
   :npermutations-repeating
   :ncombinations
   :ncombinations-repeating
   :binomial
   :multinomial
   :permutation->index
   :index->permutation
   ;; misc
   :logistic
   :logistic-derivative
   ;; utils:
   :->double-float
   :periodic-shift
   :solve-interval-bisection
   ;; macros:
   :for-permutations
   :for-permutations-repeating
   :for-combinations
   :for-combinations-repeating
   ;; integer logic:
   :logpos
   :clz
   ))

(cl-ana.gmath:use-gmath :cl-ana.math-functions)
