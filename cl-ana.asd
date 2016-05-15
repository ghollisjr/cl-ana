;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2015 Gary Hollis
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
(asdf:defsystem #:cl-ana
  :serial t
  :author "Gary Hollis"
  :description "cl-ana is a free (GPL) data analysis library in Common
  Lisp providing tabular & binned data analysis along with nonlinear
  least squares fitting & visualization."
  :license "GPLv3"
  :depends-on (#:cl-ana.pathname-utils
               #:cl-ana.package-utils
               #:cl-ana.generic-math
               #:cl-ana.math-functions
               #:cl-ana.calculus
               #:cl-ana.binary-tree
               ;; Make sure to place tensor after defining all gmath
               ;; generic functions
               #:cl-ana.tensor
               #:cl-ana.error-propogation
               #:cl-ana.quantity
               #:cl-ana.table
               #:cl-ana.table-utils
               #:cl-ana.hdf-table
               #:cl-ana.ntuple-table
               #:cl-ana.csv-table
               #:cl-ana.reusable-table
               #:cl-ana.linear-algebra
               #:cl-ana.lorentz
               #:cl-ana.histogram
               #:cl-ana.fitting
               #:cl-ana.file-utils
               #:cl-ana.statistics
               #:cl-ana.plotting
               #:cl-ana.table-viewing
               #:cl-ana.int-char
               #:cl-ana.clos-utils
               #:cl-ana.serialization
               #:cl-ana.hash-table-utils
               #:cl-ana.map
               ;; makeres
               #:cl-ana.makeres
               #:cl-ana.makeres-macro
               #:cl-ana.makeres-block
               #:cl-ana.makeres-progress
               #:cl-ana.makeres-table
               #:cl-ana.makeres-graphviz
               #:cl-ana.makeres-branch
               #:cl-ana.makeres-utils)
  :components ((:file "package")))
