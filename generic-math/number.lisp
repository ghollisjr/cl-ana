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

(in-package :cl-ana.generic-math)

;;;; Implements generic math functions for numbers

;;; I know I could create a macro to automate this redundant code,
;;; maybe I will at some point, but I don't want to obfuscate the code
;;; too much for the poor readers.

(declaim (optimize (speed 3)
                   (safety 0)
                   (compilation-speed 0)
                   (debug 0)))

(defmethod add ((x number) (y number))
  (cl:+ x y))

(defmethod sub ((x number) (y number))
  (cl:- x y))

(defmethod unary-sub ((x number))
  (cl:- x))

(defmethod mult ((x number) (y number))
  (cl:* x y))

(defmethod div ((x number) (y number))
  (cl:/ x y))

(defmethod unary-div ((x number))
  (cl:/ x))

(defmethod protected-div (x (y number)
			  &key
			    (protected-value 0))
  (if (zerop y)
      protected-value
      (div x y)))

(defmethod protected-unary-div ((x number)
				&key
				  (protected-value 0))
  (if (zerop x)
      protected-value
      (unary-div x)))

(defmethod sqrt ((x number))
  (cl:sqrt x))

(defmethod expt ((x number) (y number))
  (cl:expt x y))

(defmethod exp ((x number))
  (cl:exp x))

(defmethod log ((x number))
  (cl:log x))

(defmethod sin ((x number))
  (cl:sin x))

(defmethod cos ((x number))
  (cl:cos x))

(defmethod tan ((x number))
  (cl:tan x))

;; methods for sec, csc, cot are handled well by defaults

(defmethod sinh ((x number))
  (cl:sinh x))

(defmethod cosh ((x number))
  (cl:cosh x))

(defmethod tanh ((x number))
  (cl:tanh x))

;; methods for sech, csch, tanh are handled well by defaults

;;; can add more as I think of them/look up what should be included
