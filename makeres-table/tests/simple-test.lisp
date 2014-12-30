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

(require 'cl-ana)

(in-package cl-ana)

(in-project simple-test)

(settrans (list #'tabletrans) :op :set)

(defres source
  (wrap-for-reuse
   (open-plist-table
    (loop
       for i below 10000000
       collecting (list :x i)))))

(deflfields source
    ((y (* 2 (field x)))))

(defres sum
  (dotab (res source)
      ((sum (progn
              (format t "starting sum~%")
              0)))
      sum
    (incf sum (field x))))

(defres count
  (dotab (res source)
      ((count (progn
                (format t "starting count~%")
                0)))
      count
    (incf count)))

(defres mean
  (format t "starting mean~%")
  (/ (res sum)
     (res count)))

(defres filtered
  (ltab (res source) ()
    (when (< (field x) (res mean))
      (push-fields
       (x (field x))))))

;; works up to this point, breaks after the following:

(defres (filtered sum)
  (dotab (res filtered)
      ((sum (progn
              (format t "starting filtered sum~%")
              0)))
      sum
    (incf sum (field x))))

(defres (filtered count)
  (dotab (res filtered)
      ((count (progn
                (format t "starting filtered count~%")
                0)))
      count
    (incf count)))

(defres (filtered mean)
  (format t "starting filtered mean~%")
  (/ (res (filtered sum))
     (res (filtered count))))
