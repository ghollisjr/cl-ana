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

(in-package cl-ana)

(defproject lfield-logging-test
    "/home/ghollisjr/test/lfield-logging-test/"
  (list #'macrotrans #'tabletrans #'progresstrans)
  (fixed-cache 5))

(defres src
  (srctab (plist-opener (list (list :x 1)
                              (list :x 2)
                              (list :x 3)))))

(deflfields src
    ((y (* (field x)
           2))
     (z (/ (field y)
           (sqrt 2)))
     (w (sqrt (field z)))))

(defres xmean
  (dotab (res src)
      ((sum 0)
       (count 0))
      (/ sum count)
    (incf sum (field x))
    (incf count)))

(defres skim
  (ltab (res src) ()
    (when (< (field x)
             (field w))
      (push-fields))))

(defres (skim xmean)
  (dotab (res skim)
      ((sum 0)
       (count 0))
      (/ sum count)
    (incf sum (field x))
    (incf count)))
