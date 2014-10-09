(defpackage #:logres-table
  (:use :cl
        :makeres)
  (:export :load-project-tables
           :table-target?))

(package-utils:use-package-group :cl-ana :logres-table)
