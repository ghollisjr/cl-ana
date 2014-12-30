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

(defpackage #:cl-ana.makeres
  (:use :cl
        :cl-ana.macro-utils
        :cl-ana.list-utils
        :cl-ana.symbol-utils
        :cl-ana.map
        :cl-ana.hash-table-utils)
  (:export
   ;; target
   :target
   :target-id
   :target-expr
   :target-deps
   :target-pdeps
   :target-val
   :target-stat
   :make-target
   :copy-target
   ;; propogation:
   :res-dependents
   :makeres-set-auto-propogate
   :makeres-propogate!
   :makeres-set-sticky-pars
   ;; hash tables (these are for debugging only
   :*symbol-tables
   :*target-tables*
   :*fin-target-tables*
   :*project-id*
   :*transformation-table*
   :*params-table*
   :*args-tables*
   :*makeres-args*
   ;; noisy messages:
   :*makeres-warnings*
   ;; functions accessing hash tables
   :project
   :project-parameters
   :project-targets
   :symbol-table
   :target-table
   :copy-target-table
   ;; dependency sorting:
   :dep<
   :depsort
   :depsort-graph
   ;; target and parameter macros
   :res
   :resfn
   :par
   :parfn
   ;; project macros
   :in-project
   :defpars
   :undefpars
   :defres
   :undefres
   :setresfn
   :setres
   :unsetresfn
   :unsetres
   :clrres
   :clrresfn
   :settrans ; set transformation pipeline
   :makeres ; compile and call result generator
   ;; project utilities
   :target-ids
   :fin-target-ids
   ;; INCLUDED TRANSFORMATIONS:
   :lrestrans ; allows logical results
   :lres))

(cl-ana.gmath:use-gmath :cl-ana.makeres)
