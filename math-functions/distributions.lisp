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

(defmath erf (x)
  (:documentation "the error function")
  (:method ((x number))
    (gsll:erf (->double-float x))))

(defmath normal-pdf (x)
  (:documentation "Standard normal probability density function")
  (:method ((x number))
    (/ (exp (/ (- (* x x))
               2))
       (sqrt (* 2
                pi)))))

(defmath normal-cdf (x)
  (:documentation "Standard normal cumulative density function")
  (:method ((x number))
    (gsll:gaussian-p (->double-float x) 1d0)))

(defmath normal-cdf-inv (x)
  (:documentation "Inverse of normal CDF")
  (:method ((x number))
    (gsll:gaussian-pinv (->double-float x) 1d0)))
