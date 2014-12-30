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
(asdf:defsystem #:cl-ana.quantity
  :serial t
  :description "Provides basic physics functions such as dimensional
  analysis, units, and some common definitions."
  :author "Gary Hollis"
  :license ""
  :depends-on (#:alexandria
               #:cl-ana.list-utils
               #:cl-ana.macro-utils
               #:cl-ana.symbol-utils
               #:cl-ana.generic-math
               #:cl-ana.error-propogation)
  :components ((:file "package")
               (:file "unit")
               (:file "macros")
               (:file "quantity")
               (:file "binary")
               (:file "si")
               (:file "metric-non-si")
               (:file "us")
               (:file "constants")
               (:file "temperature")
               (:file "post-constants")
               (:file "convert-units")))
