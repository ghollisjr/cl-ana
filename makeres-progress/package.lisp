;;;; makeres-progress is a Common Lisp make-like tool for computations.
;;;; Copyright 2015 Gary Hollis
;;;; 
;;;; This file is part of makeres-progress.
;;;; 
;;;; makeres-progress is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; makeres-progress is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-progress.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(defpackage #:cl-ana.makeres-progress
  (:use :cl
        :cl-ana.makeres)
  (:export :progresstrans
           :*makeres-progress-results-only-p*))

(cl-ana.gmath:use-gmath :cl-ana.makeres-progress)
