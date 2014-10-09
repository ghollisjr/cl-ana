(defpackage #:logres-table
  (:use :cl
        :reusable-table
        :table
        :makeres)
  (:export :load-project-tables
           :table-target?))
