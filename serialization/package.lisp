(defpackage #:serialization
  (:use :cl
        :histogram
        :hdf-utils
        :typespec
        :table
        :hdf-table)
  (:export :write-histogram
           :read-histogram))
