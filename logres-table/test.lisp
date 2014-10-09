(require 'logres)
(require 'logres-table)
(require 'makeres-table)
(require 'makeres-macro)

(defpackage #:logres-table-test
  (:use :cl
        :makeres
        :makeres-macro
        :makeres-table
        :logres
        :logres-table))

(package-utils:use-package-group :cl-ana :logres-table-test)

(in-package :logres-table-test)

(in-project logres-table-test)
(settrans (list #'macrotrans #'tabletrans)
          :op :set)

(set-project-path "logres-table-test/")
(logres-ignore-by #'table-target?)

(defres source
  (wrap-for-reuse
   (open-plist-table (list (list :x 1)
                           (list :x 2)
                           (list :x 3)))))

(defres (filter source)
  (tab (res source)
      ()
      (hdf-opener (merge-pathnames "work/filter-source.h5"
                                   (project-path))
                  (list (cons "x" :int)))
    (when (< (field x) 3)
      (push-fields (x (field x))))))

(define-res-macro tabmean (table field)
  `(dotab ,table
       ((sum 0)
        (count 0))
       (/ sum count)
     (incf sum (field ,field))
     (incf count)))

(defres (mean x (filter source))
  (tabmean (res (filter source))
           x))

(defun load-ana (version)
  (load-project version)
  (load-project-tables))
