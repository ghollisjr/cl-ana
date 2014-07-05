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
   ;; hash tables
   :*symbol-tables
   :*target-tables*
   :fin-target-tables*
   :*project-id*
   :*transformation-table*
   :*params-table*
   :*args-tables*
   ;; functions accessing hash tables
   :project
   :symbol-table
   :target-table
   ;; dependency sorting:
   :dep<
   :depsort
   ;; target and parameter macros
   :res
   :par
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
   :fin-target-ids))

(package-utils:use-package-group :cl-ana :makeres)
