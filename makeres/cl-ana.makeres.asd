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
;;;; along with makeres.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(asdf:defsystem #:cl-ana.makeres
  :serial t
  :author "Gary Hollis"
  :description "makeres is a make-like tool for building analysis
  results in Common Lisp"
  :license "GPLv3"
  :depends-on (#:alexandria
               #:cl-fad
               #:cl-ana.memoization
               #:cl-ana.generic-math
               #:cl-ana.error-propogation
               #:cl-ana.macro-utils
               #:cl-ana.list-utils
               #:cl-ana.symbol-utils
               #:cl-ana.map
               #:cl-ana.hash-table-utils
               #:cl-ana.plotting
               ;; logres:
               #:external-program
               #:cl-ana.hdf-utils
               #:cl-ana.serialization
               #:cl-ana.string-utils
               #:cl-ana.functional-utils
               #:cl-ana.file-utils
               #:cl-ana.histogram
               #:cl-ana.pathname-utils
               #:cl-ana.table
               #:cl-ana.reusable-table)
  :components ((:file "package")
               (:file "makeres")
               (:file "lrestrans")
               ;; logres:
               (:file "logres")
               (:file "histogram")
               (:file "function")
               (:file "hash-table")
               (:file "cons")
               (:file "array")
               (:file "string")
               (:file "err-num")))
