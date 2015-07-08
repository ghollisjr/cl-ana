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

(asdf:defsystem #:cl-ana.makeres-branch
  :serial t
  :author "Gary Hollis"
  :description "makeres-branch is a graph transformation for use with
  cl-ana.makeres"
  :license "GPLv3"
  :depends-on (#:alexandria
               #:cl-ana.generic-math
               #:cl-ana.makeres
               #:cl-ana.list-utils
               #:cl-ana.map
               #:cl-ana.hash-table-utils)
  :components ((:file "package")
               (:file "makeres-branch")))
