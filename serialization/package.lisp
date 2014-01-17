(defpackage #:serialization
  (:use :cl
        :histogram
        :table
        :hdf-table)
  (:export :write-histogram
           :read-histogram))
