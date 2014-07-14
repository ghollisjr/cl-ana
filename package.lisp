(defpackage #:makeres-tabletrans
  (:use :cl
        :makeres)
  (:export
   ;; table reduction operators:
   :dotab
   :ltab
   :tab
   ;; field macro:
   :field
   ;; implementation macro:
   :table-pass
   ;; transformations
   :tabletrans
   :pass-merge
   :pass-collapse
   ;; hash tables
   :*tabletrans-symmap*
   ;; hdf-utils:
   :hdf-opener))

(package-utils:use-package-group :cl-ana :makeres-tabletrans)
