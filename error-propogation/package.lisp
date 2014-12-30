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
;;;; package.lisp

(defpackage #:cl-ana.error-propogation 
  (:nicknames #:cl-ana.err-prop)
  (:use :cl)
  (:export :*err-num-pretty-print* ; for print style
	   :err-num
	   :+-
	   :err-num-value
	   :err-num-error
	   ;; These functions are provided in the event that one wants
	   ;; to supply a lot of arguments to the generic +, -, *, or
	   ;; / functions, since this would be inefficient the way it
	   ;; is implemented currently:
	   :err-num-+
	   :err-num--
	   :err-num-*
	   :err-num-/))

(cl-ana.gmath:use-gmath :cl-ana.error-propogation)
