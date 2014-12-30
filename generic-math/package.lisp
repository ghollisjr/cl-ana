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

(defpackage #:cl-ana.generic-math 
  (:nicknames :cl-ana.gmath)
  (:use :cl
        :cl-ana.package-utils
	:cl-ana.list-utils)
  (:shadow :incf
	   :decf
	   :+
	   :-
	   :*
	   :/
	   :sqrt
	   :expt
	   :exp
	   :log
	   :sin
	   :cos
	   :tan
	   :sinh
	   :cosh
	   :tanh)
  (:export :*gmath-generic-map*
           :use-gmath
           :defmath
	   :defmethod-commutative
           :incf
           :decf
	   :+
	   :sum
           :product
	   :add
	   :-
	   :sub
	   :unary-sub
	   :*
	   :mult
	   :/
	   :protected-/
	   :div
	   :unary-div
	   :protected-div
	   :protected-unary-div
	   :sqrt
	   :expt
	   :exp
	   :log
	   :sin
	   :cos
	   :tan
	   :sec
	   :csc
	   :cot
	   :sinh
	   :cosh
	   :tanh
	   :sech
	   :csch
	   :coth))
