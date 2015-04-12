;;;; makeres-block is a Common Lisp make-like tool for computations.
;;;; Copyright 2014 Gary Hollis
;;;; 
;;;; This file is part of makeres-block.
;;;; 
;;;; makeres-block is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; makeres-block is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-block.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(defsystem #:cl-ana.makeres-block
  :serial t
  :author "Gary Hollis"
  :description "makeres-block is a makeres graph transformation which
  allows multiple results to be defined by a single block of code."
  :license "GPLv3"
  :depends-on (#:alexandria
               #:cl-ana.macro-utils
               #:cl-ana.list-utils
               #:cl-ana.makeres
               ;; #:cl-ana.symbol-utils
               ;; #:cl-ana.map
               ;; #:cl-ana.hash-table-utils
               )
  :components ((:file "package")
               (:file "makeres-block")))
