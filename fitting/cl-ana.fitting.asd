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
;;;; fitting.asd

(asdf:defsystem #:cl-ana.fitting
  :serial t
  :description "Library for non-linear least squares fitting (works
  for linear functions as well though); makes use of GSLL's fit
  functions."
  :author "Gary Hollis"
  :license "GPLv3"
  :depends-on (#:gsll
               #:alexandria
	       #:cl-ana.generic-math
               #:cl-ana.math-functions
	       #:cl-ana.error-propogation
	       #:cl-ana.map)
  :components ((:file "package")
	       (:file "fitting")
	       (:file "functions")
               (:file "guess-fit-params")))
