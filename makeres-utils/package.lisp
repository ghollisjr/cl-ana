;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2016 Gary Hollis
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

(defpackage #:cl-ana.makeres-utils
  (:use :cl
        :cl-ana.fitting
        :cl-ana.makeres
        :cl-ana.macro-utils
        :cl-ana.list-utils
        :cl-ana.symbol-utils
        :cl-ana.map
        :cl-ana.plotting
        :cl-ana.string-utils
        :cl-ana.functional-utils
        :cl-ana.file-utils
        :cl-ana.histogram
        :cl-ana.pathname-utils
        :cl-ana.table
        :cl-ana.reusable-table)
  (:export
   ;; Basic utilities:
   :deffitres
   :fit-results
   :fit-params
   :fit-plot
   ))

(cl-ana.gmath:use-gmath :cl-ana.makeres-utils)
