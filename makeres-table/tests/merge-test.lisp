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

;;;; This file demonstrates a bug when you
;;;;
;;;; 1. Run the project
;;;; 2. Restart Lisp
;;;; 3. Load the project
;;;; 4. (makeres)
;;;;
;;;; There seems to be a problem in pass merging.

(require 'cl-ana)

(in-package :cl-ana)

(defproject merge-test "/home/ghollisjr/test/merge-test"
  (list #'macrotrans #'tabletrans #'progresstrans)
  (fixed-cache 1))

(setf *print-progress* nil)

(defres src
  (srctab (plist-opener '((:x 1 :y 2)
                          (:x 2 :y 3)))))

(defres (src sum)
  (dotab (res src)
      ((sum 0))
      sum
    (incf sum (+ (field x)
                 (field y)))))
;;(logres-ignore (src sum))

(defres (src sum2)
  (expt (res (src sum))
        2))
;;(logres-ignore (src sum2))

(defres proc
  (tab (res src) ()
      (csv-opener (work-path "proc.csv")
                  :field-names
                  (list "X"
                        "Y"
                        "Z")
                  :read-from-string t)
    (push-fields
     (x (field x))
     (y (field y))
     (z (/ (field x)
           (field y))))))

(defres (proc sum)
  (dotab (res proc)
      ((sum 0))
      sum
    (incf sum (+ (field x)
                 (field y)
                 (field z)))))
;;(logres-ignore (proc sum))

(defres (proc sum2)
  (expt (res (proc sum))
        2))
;;(logres-ignore (proc sum2))

(defres ult
  (tab (res proc) ()
      (csv-opener (work-path "ult.csv")
                  :field-names (list "X")
                  :read-from-string t)
    (when (res (src sum))
      (push-fields
       (x (field x))))))

(defres (ult sum)
  (dotab (res ult)
      ((sum 0))
      sum
    (incf sum (field x))))
;;(logres-ignore (ult sum))
