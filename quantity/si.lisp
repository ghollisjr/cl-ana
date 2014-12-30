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

;;; base units

(defvar +si-base-units+
  (list (cons :meter :length)
        (cons :kilogram :mass)
        (cons :second :time)
        (cons :ampere :electric-current)
        (cons :kelvin :temperature)
        (cons :mole :amount-of-substance)
        (cons :candela :luminuous-intensity)))

(loop
   for (u . d) in +si-base-units+
   do (set-default-unit d u))

;;; derived units

(define-unit :hertz
    (/ :second))

(define-unit :newton
    (* :kilogram
       :meter
       (expt :second -2)))

(define-unit :pascal
    (/ :newton
       (expt :meter 2)))

(define-unit :joule
    (* :newton
       :meter))

(define-unit :watt
    (/ :joule
       :second))

(define-unit :coulomb
    (* :ampere :second))

(define-unit :volt
    (/ :watt
       :ampere))

(define-unit :farad
    (/ :coulomb
       :volt))

(define-unit :ohm
    (/ :volt
       :ampere))

(define-unit :siemens
    (/ :ampere
       :volt))

(define-unit :weber
    (* :volt :second))

(define-unit :tesla
    (/ :weber (expt :meter 2)))

(define-unit :henry
    (/ :weber :ampere))

(define-unit :lumen
    (quantity :candela)) ;; technically there's a steradian in there,
;; but that's just 1.

(define-unit :lux
    (/ :lumen (expt :meter 2)))

(define-unit :becquerel
    (/ :second))

(define-unit :gray
    (/ :joule :kilogram))

(define-unit :sievert
    (/ :joule :kilogram))

(define-unit :katal
    (/ :mole :second))
