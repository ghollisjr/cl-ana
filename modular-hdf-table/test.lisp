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

(in-package :cl-ana)

(defun write-test ()
  (with-open-hdf-file (file "/home/ghollisjr/modular.h5"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (let ((table (create-modular-hdf-table file "/test"
                                           '(("x" . :int)
                                             ("y" . :double)))))
      (loop
         for i below 10
         do (table-push-fields table
              (x i)
              (y (->double-float (sqrt i)))))
      (table-close table))))

(defun read-test ()
  (with-open-hdf-file (file "/home/ghollisjr/modular.h5"
                            :direction :input
                            :if-does-not-exist :error)
    (let ((table (open-modular-hdf-table file "/test")))
      (do-table (row-index table)
          ("x"
           "y")
        (format t "x: ~a, y: ~a~%" x y)))))

;; comparison

(defun modular-table-test ()
  (with-open-hdf-file (file "/home/ghollisjr/modular.h5"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (let ((table
           (create-modular-hdf-table file "/test"
                                     (list (cons "x" :int)
                                           (cons "y" :int))
                                     :buffer-size 10000)))
      (dotimes (i 100000)
        (table-push-fields table
          (x i)
          (y i)))
      (table-close table)))
  
  (time (with-open-hdf-file (file "/home/ghollisjr/modular.h5"
                                  :direction :input)
          (let ((table
                 (open-modular-hdf-table file "/test"))
                (sum 0))
            (do-table (row-index table)
                ("x")
              (incf sum x))
            (table-close table)
            (print sum)))))

(defun hdf-table-test ()
  (with-open-hdf-file (file "/home/ghollisjr/hdf.h5"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (let ((table
           (create-hdf-table file "/test"
                             (list (cons "x" :int)
                                   (cons "y" :int))
                             :buffer-size 10000)))
      (dotimes (i 100000)
        (table-push-fields table
          (x i)
          (y i)))
      (table-close table)))
  
  (time (with-open-hdf-file (file "/home/ghollisjr/hdf.h5"
                                  :direction :input)
          (let ((table
                 (open-hdf-table file "/test"))
                (sum 0))
            (do-table (row-index table)
                ("x")
              (incf sum x))
            (table-close table)
            (print sum)))))
