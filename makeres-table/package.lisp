(defpackage #:cl-ana.makeres-table
  (:use :cl
        :cl-ana.memoization
        :cl-ana.list-utils
        :cl-ana.macro-utils
        :cl-ana.table
        :cl-ana.reusable-table
        :cl-ana.hdf-utils
        :cl-ana.csv-table
        :cl-ana.ntuple-table
        :cl-ana.hdf-table
        :cl-ana.hash-table-utils
        :cl-ana.string-utils
        :cl-ana.makeres
        :cl-ana.makeres-macro)
  (:export
   ;; table reduction operators:
   :srctab
   :dotab
   :ltab
   :tab
   ;; field macros
   :deflfields
   :deflfieldsfn
   :field
   :push-fields
   ;; implementation macro:
   :table-pass
   ;; transformations
   :tabletrans
   ;; openers:
   :hdf-opener
   :hdf-chain-opener
   :ntuple-opener
   :csv-opener
   :plist-opener
   ;; macro expansion:
   :ensure-table-binding-ops
   :ensure-table-op-expanders
   ;; progress printing:
   :*print-progress*
   ;; expression utilities:
   :resform?
   :unres
   :mkres
   :table-reduction?
   :tab?
   :ltab?
   :dotab?
   :table-reduction-source
   ;; Special operators:
   :defhist
   ;; Copy utils:
   :copy-lfields
   ;; Utilities:
   :table-target?
   ;;; For implementing other tabletrans-like operators:
   ;; Backbone of the algorithm, grouping ids into passes
   :group-ids-by-pass
   ;; source tables, immediate, and necessary reductions
   :ultimate-source-tables
   :immediate-reductions
   :necessary-pass-reductions
   ;; reduction chains
   :chainmap
   :invert-chainmap
   :chained-edge-map
   :chained-reductions
   ;; ltab chains
   :ltab-chainmap
   :ltab-chain-edge-map
   :ltab-chained-reductions
   :ltab-chains
   ;; Dependency map functions for use with group-ids-by-pass
   :invert-depmap
   :removed-source-depmap
   :removed-source-dep<
   :removed-ltab-source-depmap
   :removed-ltab-source-dep<
   ))
