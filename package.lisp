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
  (list :package-utils
        :generic-math
        :binary-tree
        :csv-table
        :math-functions
        :tensor
        :error-propogation
        :file-utils
        :fitting
        :functional-utils
        :gnuplot-i-cffi
        :gsl-cffi
        :hdf-cffi
        :hdf-table
        :hdf-typespec
        :hdf-utils
        :histogram
        :linear-algebra
        :list-utils
        :lorentz
        :macro-utils
        :map
        :memoization
        :ntuple-table
        :plotting
        :reusable-table
        :statistics
        :string-utils
        :symbol-utils
        :symbol-utils
        :table
        :table-viewing
        :typed-table
        :typespec
        :int-char
        :clos-utils
        :serialization))

(loop
   for p in *cl-ana-package-names*
   do (package-utils:add-package-to-group p :cl-ana))

(package-utils:use-package-group :cl-ana)
