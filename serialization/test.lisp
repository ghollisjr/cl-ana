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

(require 'cl-ana)

(in-package :cl-ana)

(defun write-histogram-test ()
  (with-open-hdf-file (file "/home/ghollisjr/hist.h5"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (write-histogram
     (make-contiguous-hist (list (list :name "x"
                                       :low -5d0
                                       :high 5d0
                                       :nbins 10)))
     file
     "/histogram")))

(defun read-histogram-test ()
  (with-open-hdf-file (file "/home/ghollisjr/hist.h5"
                            :direction :input
                            :if-does-not-exist :error)
    (read-histogram file "/histogram")))
