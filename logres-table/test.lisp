;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
;;;; 
;;;; This file is part of cl-ana.
;;;; 
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(require 'cl-ana.logres)
(require 'cl-ana.logres-table)
(require 'cl-ana.makeres-table)
(require 'cl-ana.makeres-macro)

(defpackage #:cl-ana.logres-table-test
  (:use :cl
        :makeres
        :makeres-macro
        :makeres-table
        :logres
        :logres-table))

(cl-ana.package-utils:use-package-group :cl-ana :cl-ana.logres-table-test)

(in-package :cl-ana.logres-table-test)

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
