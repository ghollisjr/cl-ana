;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2023 Gary Hollis
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

;; These are the Troy weight system of units

(define-unit :grain
    (* 64.79891 (milli :gram)))

(define-unit :pennyweight
    (* 24 :grain))

(define-unit :troy-ounce
    ;; could also be defined as
    ;; (* 20 :pennyweight)
    (* 31.1034768 :gram))

(define-unit :troy-pound
    (* 12 :troy-ounce))
