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

;;;; Customary units:

(define-unit :inch
    (* 2.54d0 (centi :meter)))

(define-unit :foot
    (* 12 :inch))

(define-unit :yard
    (* 3 :foot))

(define-unit :mile
    (* 5280 :foot))

(define-unit :nautical-mile
    (* 1.151d0 :mile))

;; (define-unit :acre
;;     #q(4046.873d0 (:meter 2)))

(define-unit :acre
    (* 4046.873d0 (expt :meter 2)))

;;;; Fluid volumes:

(define-unit :teaspoon
    (* 4.928921 (milli :liter)))

(define-unit :tablespoon
    (* 3 :teaspoon))

(define-unit :fluid-ounce
    (* 2 :tablespoon))

(define-unit :cup
    (* 8 :fluid-ounce))

(define-unit :pint
    (* 2 :cup))

(define-unit :quart
    (* 2 :pint))

(define-unit :gallon
    (* 4 :quart))

;;;; Mass

(define-unit :pound
    (* 453.59237d0 :gram))

(define-unit :grain
    (/ :pound 7000))

(define-unit :dram
    (* (+ 27 11/32)
       :grain))

(define-unit :ounce
    (* 16 :dram))

(define-unit :ton
    (* 2000 :pound))

(define-unit :long-ton
    (* 2240 :pound))

;; Speed

(define-unit :mph
    (/ :mile :hour))

;; Miscellaneous

;; U.S. commonly used size for a "cup" of coffee
(define-unit :coffee-cup
    (* 6 :fluid-ounce))
