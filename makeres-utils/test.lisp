;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2016 Gary Hollis
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

(require 'cl-ana)
(in-package :cl-ana)

(defproject makeres-utils-test "/home/ghollisjr/test/makeres-utils-test"
  (list #'macrotrans #'progresstrans)
  (fixed-cache 5))

(defres data
  '((1 . 2)
    (2 . 3)
    (3 . 5)
    (4 . 5)
    (6 . 4)))

(deffitres data
    (res data)
  #'polynomial
  :force-p t
  :x-range (cons 0 7)
  :y-range (cons 0 7)
  :draw-p t
  :nparams 2
  :flags (linear))
