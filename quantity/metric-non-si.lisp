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

(in-package :cl-ana.quantity)

;; These are common metric units which do not officially belong to the
;; S.I. unit system.

(define-unit :gram
    (milli :kilogram))

(define-unit :minute
    (* 60 :second))

(define-unit :hour
    (* 60 :minute))

(define-unit :day
    (* 24 :hour))

(define-unit :year
    (* 365.25 :day))

(define-unit :hectare
    (expt (hecto :meter) 2))

(define-unit :liter
    (expt (deci :meter) 3))

(define-unit :tonne
    (expt (mega :gram)))

(define-unit :atomic-mass-unit
    (* 1.6605388628d-27 :kilogram))

(define-unit :dalton :atomic-mass-unit)

(define-unit :angstrom
    (/ (nano :meter) 10))

(define-unit :barn
    (* 1d-28
       (expt :meter 2)))

(define-unit :bar
    (* 1d5 :pascal))

(define-unit :atmosphere
    (* 101325 :pascal))

(define-unit :mmHg ;; the full name is way too long
    (* 133.322387415d0 :pascal))

(define-unit :torr
    (* 133.322368421d0 :pascal))

(define-unit :kph
    (/ (kilo :meter) :hour))

(define-unit :rpm
    (/ (* 2 pi)
       :minute))
