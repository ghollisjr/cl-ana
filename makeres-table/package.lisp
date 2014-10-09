(defpackage #:makeres-table
  (:use :cl
        :list-utils
        :macro-utils
        :table
        :reusable-table
        :hdf-utils
        :csv-table
        :ntuple-table
        :hdf-table
        :hash-table-utils
        :string-utils
        :makeres
        :makeres-macro)
  (:export
   ;; table reduction operators:
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
   :ntuple-opener
   :csv-opener
   :plist-opener
   ;; macro expansion:
   :ensure-table-binding-ops
   :ensure-table-op-expanders
   ;; progress printing:
   :*print-progress*
   ;; expression utilities:
   :table-reduction?
   :tab?
   :ltab?
   :dotab?
   :table-reduction-source))
