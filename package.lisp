(defpackage #:makeres
  (:use :cl)
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
   ;; Variable for propogation setting:
   :*makeres-propogate*
   ;; hash tables (these are for debugging only
   :*symbol-tables
   :*target-tables*
   :*fin-target-tables*
   :*project-id*
   :*transformation-table*
   :*params-table*
   :*args-tables*
   :*makeres-args*
   ;; functions accessing hash tables
   :project
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
   :setresfn
   :setres
   :unsetresfn
   :unsetres
   :clrres
   :clrresfn
   :settrans ; set transformation pipeline
   :compres ; compile result generator
   :makeres ; compile and call result generator
   ;; project utilities
   :target-ids
   :fin-target-ids
   ;; INCLUDED TRANSFORMATIONS:
   :lrestrans)) ; allows logical results

(package-utils:use-package-group :cl-ana :makeres)
