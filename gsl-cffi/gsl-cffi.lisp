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

(in-package :cl-ana.gsl-cffi)

(defconstant +GSL-EOF+ 32
  "End-of-file return value for gsl functions")

(define-foreign-library gsl-cffi
  (t (:default "libgsl")))

(defcfun "gsl_ntuple_read" :int
  (ntuple :pointer)) ; gsl_ntuple*

;;; Currently unused:

(defcfun "gsl_multifit_fsolver_alloc" :pointer
  (gsl-multifit-fsolver-type :pointer) ; gsl_multifit_fsolver_type*
  (n-data :uint)
  (n-params :uint))

(defcfun "gsl_multifit_fsolver_free" :void
  (gsl-multifit-fsolver :pointer)) ; gsl_multifit_solver*

(defcfun "gsl_multifit_fsolver_set" :int
  (solver :pointer) ; gsl_multifit_fsolver*
  (gsl-multifit-function :pointer) ; gsl_multifit_function*
  (initial-guess :pointer)) ; const gsl_vector*
