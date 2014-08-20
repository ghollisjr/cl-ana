(defpackage #:makeres-table
  (:use :cl
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
   :plist-opener))

(package-utils:use-package-group :cl-ana :makeres-table)
