;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2019 Katherine Cox-Buday
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

(asdf:defsystem #:cl-ana.columnar-table
  :serial t
  :license "GPLv3"
  :author "Gary Hollis"
  :description "Used to access tables which have been pivoted so that
  a rows values are stratified across all rows, and a column's values
  are accessible all from a single row."
  :depends-on (#:cl-ana.table
               #:cl-ana.reusable-table)
  :components ((:file "package")
               (:file "columnar-table")))
