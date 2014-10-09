;;;; makeres is a Common Lisp make-like tool for computations.
;;;; Copyright 2014 Gary Hollis
;;;; 
;;;; This file is part of makeres.
;;;; 
;;;; makeres is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; makeres is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(defsystem #:makeres
  :serial t
  :author "Gary Hollis"
  :description "makeres is a make-like tool for building analysis
  results in Common Lisp"
  :license "GPLv3"
  :depends-on (#:alexandria
               #:list-utils
               #:symbol-utils
               #:map
               #:hash-table-utils)
  :components ((:file "package")
               (:file "makeres")
               (:file "lrestrans")))
