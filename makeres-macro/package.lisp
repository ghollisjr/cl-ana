;;;; makeres-macro is a Common Lisp make-like tool for computations.
;;;; Copyright 2014 Gary Hollis
;;;; 
;;;; This file is part of makeres-macro.
;;;; 
;;;; makeres-macro is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; makeres-macro is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-macro.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(defpackage #:cl-ana.makeres-macro
  (:use :cl
        :cl-ana.list-utils
        :cl-ana.makeres)
  (:export :define-res-macro
           :define-res-function
           :expand-res-macros
           :add-binding-ops
           :macrotrans
           :ensure-binding-ops
           :*proj->binding-ops*
           :ensure-op-expanders
           :*proj->op->expander*))
