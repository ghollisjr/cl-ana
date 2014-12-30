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
;;;; map.lisp

(in-package :cl-ana.map)

;;;; A map is any object which represents or can be represented by the
;;;; mapping from one value to another; a two good examples are alists
;;;; and hash tables.
;;;;
;;;; These utilities are for the generic handling of maps so they can
;;;; be appropriately treated by things like fitting data or making
;;;; scatter plots.

(defgeneric map->alist (object)
  (:documentation "Returns the values of a data-source as an alist
  mapping the independent value to the dependent one.
  Multidimensional independent variable data can be represented as a
  list of values.")
  (:method ((obj list))
    obj)
  (:method ((obj hash-table))
    (hash-table->alist obj)))

(defun map->hash-table (object &optional test)
  (alist->hash-table
   (map->alist object)
   test))
