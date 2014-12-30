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

;;;; Energy:

(define-unit :electron-volt
    (* +e+ :volt))

(define-unit :dyne
    (* 10 (micro :newton)))

(define-unit :calorie
    (* 4.184d0 :joule))

;; Food calorie
(define-unit :cal
    (kilo :calorie))

;;;; U.S. force & power
(define-unit :pound-force
  (* +graviational-acceleration+ :pound))

(define-unit :horsepower
    (* 33000 :foot :pound-force (/ :minute)))

;;;; Astronomical Distance:

(define-unit :light-year
    (* +c+ :year))

(define-unit :astronomical-unit
    (* 149597870.700d0
       (kilo :meter)))

(define-unit :parsec
    (/ :astronomical-unit
       (tan (/ pi
               (* 180 3600)))))
