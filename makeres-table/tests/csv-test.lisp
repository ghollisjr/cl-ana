;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2015 Gary Hollis
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

(require 'cl-ana)

(in-package :cl-ana)

;; Basic tests

(defun csv-test ()
  (let* ((opener
          (csv-opener "/home/ghollisjr/test.csv"
                      (list "a")
                      :read-from-string t))
         tab)
    (setf tab
          (funcall opener :write))
    (loop
       for i below 5
       do (table-push-fields tab
            (|a| i)))
    (setf tab (funcall opener :read))
    tab))

;; Project test

(defproject csv-test
    "/home/ghollisjr/test/makeres-table/csv-test"
  (list #'macrotrans #'branchtrans #'tabletrans #'progresstrans)
  (fixed-cache 5))

(setf *print-progress* nil)

(defres bootstrap
  (let ((table
         (create-csv-table (work-path "src.csv")
                           (list "X" "Y" "Z"))))
    (loop
       for i below 100
       do (let* ((x (->double-float i))
                 (y (sqrt x))
                 (z (sqrt y)))
            (table-push-fields table
              (x x)
              (y y)
              (z z))))
    (table-close table)))

(defres src
  (srctab (csv-opener (work-path "src.csv")
                      (list "X" "Y" "Z")
                      :read-from-string t)
          (res bootstrap)))

(defres proc
  (tab (res src) ()
      (csv-opener (work-path "proc.csv")
                  (list "X" "Y" "Z" "W" "Q")
                  :read-from-string t)
    (when (< (field x) 50)
      (push-fields
       (x (field x))
       (y (field y))
       (z (field z))
       (w (+ (field x)
             (field y)
             (field z)))
       (q (* (field x)
             (field y)
             (field z)))))))

(defres subset
  (tab (res proc) ()
      (csv-opener (work-path "subset.csv")
                  (list "X" "Y")
                  :read-from-string t)
    (when (< (field x) 4)
      (push-fields
       (x (field x))
       (y (field y))))))

(defres subset-data
  (dotab (res subset)
      ((data nil))
      (nreverse data)
    (push (list :x (field x)
                :y (field y))
          data)))
