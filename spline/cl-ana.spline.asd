;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2021 Gary Hollis
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

(asdf:defsystem #:cl-ana.spline
  :serial t
  :author "Gary Hollis"
  :description "Provides natural splines to arbitrary order using GSL."
  :license "GPLv3"
  :depends-on (#:gsll
               #:cffi
               #:cl-ana.generic-math
               #:cl-ana.macro-utils
               #:cl-ana.list-utils
               #:cl-ana.tensor
               #:cl-ana.math-functions
               #:cl-ana.fitting
               ;; #:alexandria
               ;; #:cl-fad
               ;; #:cl-ana.memoization
               ;; #:cl-ana.generic-math
               ;; #:cl-ana.error-propogation
               ;; #:cl-ana.macro-utils
               ;; #:cl-ana.list-utils
               ;; #:cl-ana.symbol-utils
               ;; #:cl-ana.map
               ;; #:cl-ana.hash-table-utils
               ;; #:cl-ana.plotting
               )
  :components ((:file "package")
               (:file "spline")))
