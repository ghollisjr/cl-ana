(defpackage #:makeres-tabletrans
  (:use :cl
        :makeres)
  (:export
   ;; general table reduction:
   :dotab
   ;; implementation macro:
   :table-pass
   ;; transformations
   :pass-merge
   :pass-collapse
   ;; hash tables
   :*tabletrans-symmap*
   ;; hdf-utils:
   :hdf-opener))

(package-utils:use-package-group :cl-ana :makeres-tabletrans)
