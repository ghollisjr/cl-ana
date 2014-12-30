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

(require 'cl-ana.reusable-table)

(require 'cl-ana.hdf-table)

(in-package :cl-ana.hdf-table)

(use-package 'cl-ana.reusable-table)

(defvar *table*
  (wrap-for-reuse
   (open-hdf-table-chain (list "/home/ghollisjr/hdfwork/outfile.h5")
                         "/output-dataset")))

(do-table (i *table*)
    ("x" "y")
  (when (zerop i)
    (print "First pass: ")
    (print x)
    (print y)))

(do-table (i *table*)
    ("x" "y")
  (when (zerop i)
    (print "Second pass: ")
    (print x)
    (print y)))
