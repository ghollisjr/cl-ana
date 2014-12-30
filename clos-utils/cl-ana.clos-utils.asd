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
(asdf:defsystem #:cl-ana.clos-utils
  :serial t
  :description "clos-utils provides various lacking utilities for
  working with structures and CLOS class instances as well as
  utilities for transforming them into clists which can be
  written/read with standard stream objects portably."
  :author "Gary Hollis"
  :license "GPLv3"
  :depends-on (#:closer-mop
               #:cl-ana.list-utils
               #:cl-ana.symbol-utils
               #:cl-ana.tensor)
  :components ((:file "package")
               (:file "clos-utils")))
