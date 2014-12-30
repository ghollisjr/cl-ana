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

(require 'cl-ana.typespec)

(in-package :cl-ana.typespec)

(defparameter *ts* '(:compound
                     ("x" . (:array :double 3 3))
                     ("y" . :string)))

(defparameter *struct* 
  '(:x ((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0))
    :y "hello"))

(defparameter *x* (typespec-foreign-alloc *ts*))

(defparameter *lisp->c-converter* (typespec->lisp-to-c *ts*))

(funcall *lisp->c-converter* *struct* *x*)

(defparameter *c->lisp-converter*
  (typespec->c-to-lisp *ts*))

(print (funcall *c->lisp-converter* *x*))
