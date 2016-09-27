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

(defpackage #:cl-ana.list-utils 
  (:use :cl
        :alexandria
        :cl-ana.string-utils
	:cl-ana.functional-utils)
  (:export :range
	   :zip
           :lzip
	   :unzip
           :unlzip
           :mapzip
           :tree-map
           :intersperse
	   :transpose
	   :cartesian-product
	   :every-nth
	   :except-nth
           :at-indices
	   :except-at
	   :compress
	   :list-less-than
	   :list-greater-than
	   :aref-by-list
	   :make-offsets
           :ensure-lists
           :partition
           ;;; alists
           :cars
           :cdrs
           ;; lists as sets
           :list->set
           ;;; plists
           :plist-select-fields
           :plist->alist
           :alist->plist
           ;; Useful looping macros over plists:
           :do-plists
           :do-plist
           ;; Paul Graham's stuff (and some of my improvements)
           :length-equal
	   :single
           :append1
           :conc1
           :mklist
           :longer
           :group
           :prune
           :find2
           :before
           :after
           :duplicate
           :permute))
