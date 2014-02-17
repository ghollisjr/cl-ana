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
;;;; package.lisp

(defpackage :histogram 
  (:use #:cl
        #:clos-utils
	#:tensor
        #:symbol-utils
        #:string-utils
	#:list-utils
	#:macro-utils
	#:iter
	#:alexandria
	#:fitting
	#:map)
  (:export :quick-hist ; useful for quickly plotting small datasets
           :histogram
           :rectangular-histogram
	   :contiguous-histogram
	   :sparse-histogram
           :categorical-histogram
	   :make-contiguous-hist
           :make-chist
	   :make-sparse-hist
           :make-shist
           :make-categorical-hist
           :make-cathist
           :sparse->contiguous
           :contiguous->sparse
	   :map-contiguous-hist
           :rectangular-hist-bin-specs
           :hist-dim-specs
	   :hist-ndims
	   :hist-dim-names
	   :hist-empty-bin-value
	   :hist-default-increment
           :hist-total-integral
	   :hist-integrate
	   :hist-project
           :hist-slice
	   :hist-insert
           :hist-insert-list
	   :hist-index-ref ; setf-able
	   :hist-point-ref ; setf-able
	   :hist-bin-values
           ;; Functional access:
           :hist-map
           :hist-filter
           ;; Abbreviations:
           :hins ;; hist-insert
           :htint ;; hist-total-integral
           :hint ;; hist-integrate
           :hproj ;; hist-project
           :hslice ;; hist-slice
           :hiref ;; hist-index-ref
           :hpref ;; hist-point-ref
           :hbv ;; hist-bin-values
           :hdn ;; hist-dim-names
           :hds)) ;; hist-dim-specs

(gmath:use-gmath :histogram)
