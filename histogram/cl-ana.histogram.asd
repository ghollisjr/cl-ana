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
;;;; histogram.asd

(asdf:defsystem #:cl-ana.histogram
  :serial t
  :author "Gary Hollis"
  :description "Support for binned data analysis via contiguous and
  sparse histograms; integrated into the generic-math framework."
  :license "GPLv3"
  :depends-on (#:iterate
               #:alexandria
               #:cl-ana.clos-utils
               #:cl-ana.symbol-utils
               #:cl-ana.list-utils
               #:cl-ana.hash-table-utils
               #:cl-ana.macro-utils
               #:cl-ana.functional-utils
	       #:cl-ana.generic-math
               #:cl-ana.binary-tree
	       #:cl-ana.map
	       #:cl-ana.tensor
	       #:cl-ana.fitting)
  :components ((:file "package")
	       (:file "histogram")
	       (:file "rectangular-histogram")
	       (:file "contiguous-histogram")
	       (:file "sparse-histogram")
               (:file "variable-binning-histogram")
               (:file "categorical-histogram")
               (:file "conversion")))
