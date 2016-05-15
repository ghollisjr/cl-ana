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

(defpackage #:cl-ana
  (:use :cl))

(in-package :cl-ana)

(defvar *cl-ana-package-names*
  (list :cl-ana.pathname-utils
        :cl-ana.package-utils
        :cl-ana.generic-math
        :cl-ana.binary-tree
        :cl-ana.csv-table
        :cl-ana.math-functions
        :cl-ana.calculus
        :cl-ana.tensor
        :cl-ana.error-propogation
        :cl-ana.quantity
        :cl-ana.file-utils
        :cl-ana.fitting
        :cl-ana.functional-utils
        :cl-ana.gnuplot-interface
        :cl-ana.gsl-cffi
        :cl-ana.hdf-cffi
        :cl-ana.hdf-table
        :cl-ana.hdf-typespec
        :cl-ana.hdf-utils
        :cl-ana.histogram
        :cl-ana.linear-algebra
        :cl-ana.list-utils
        :cl-ana.lorentz
        :cl-ana.macro-utils
        :cl-ana.map
        :cl-ana.hash-table-utils
        :cl-ana.memoization
        :cl-ana.ntuple-table
        :cl-ana.plotting
        :cl-ana.reusable-table
        :cl-ana.statistics
        :cl-ana.string-utils
        :cl-ana.symbol-utils
        :cl-ana.symbol-utils
        :cl-ana.table
        :cl-ana.table-utils
        :cl-ana.table-viewing
        :cl-ana.typed-table
        :cl-ana.typespec
        :cl-ana.int-char
        :cl-ana.clos-utils
        :cl-ana.serialization
        ;; makeres:
        :cl-ana.makeres
        :cl-ana.makeres-macro
        :cl-ana.makeres-block
        :cl-ana.makeres-progress
        :cl-ana.makeres-table
        :cl-ana.makeres-graphviz
        :cl-ana.makeres-branch
        :cl-ana.makeres-utils))

(loop
   for p in *cl-ana-package-names*
   do (cl-ana.package-utils:add-package-to-group p :cl-ana))

(cl-ana.package-utils:use-package-group :cl-ana)
