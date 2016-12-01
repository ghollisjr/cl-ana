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
   :table-target?))
