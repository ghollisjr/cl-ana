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
                                     :buffer-size 1000000)))
      (dotimes (i 1000000)
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

(defun make-table (type nrows ncols)
  (let* ((filename (if (equal type :hdf)
                       "/home/ghollisjr/hdf.h5"
                       "/home/ghollisjr/modular.h5"))
         (path "/test")
         (column-names
          (loop
             for i below ncols
             collecting
               (with-output-to-string (s)
                 (format s "x~a" i))))
         (names-specs
          (loop
             for n in column-names
             collecting (cons n :int))))
    (with-open-hdf-file (file filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((table
             (funcall (if (equal type :hdf)
                          #'create-hdf-table
                          #'create-modular-hdf-table)
                      file path names-specs)))
        (dotimes (i nrows)
          (loop
             for n in column-names
             do (table-set-field table
                                 (keywordify (lispify n))
                                 i))
          (table-commit-row table))
        (table-close table)))))

(defun read-table (type ncols)
  (let ((filename (if (equal type :hdf)
                      "/home/ghollisjr/hdf.h5"
                      "/home/ghollisjr/modular.h5"))
        (path "/test")
        (column-names
         (loop
            for i below ncols
            collecting
              (with-output-to-string (s)
                (format s "x~a" i)))))
    (with-open-hdf-file (file filename
                              :direction :input)
      (let ((table (funcall (if (equal type :hdf)
                                #'open-hdf-table
                                #'open-modular-hdf-table)
                            file path)))
        (table-reduce table column-names
                      (lambda (state &rest xs)
                        (when (and (zerop (first xs))
                                   (not (equal type :hdf)))
                          (print (modular-hdf-table::modular-hdf-table-active-fields table)))
                        (loop
                           for x in xs
                           summing x into sum
                           finally (return (+ state sum))))
                      :initial-value 0)))))

(defun hdf-table-test ()
  (with-open-hdf-file (file "/home/ghollisjr/hdf.h5"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (let ((table
           (create-hdf-table file "/test"
                             (list (cons "x" :int)
                                   (cons "y" :int))
                             :buffer-size 1000000)))
      (dotimes (i 1000000)
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

(defun modular-table-read-test ()
  (with-open-hdf-file (file "/home/ghollisjr/modular.h5"
                            :direction :input)
    (let ((table
           (open-modular-hdf-table file "/test"))
          (sum 0))
      (do-table (row-index table)
          ("x")
        (incf sum x))
      (table-close table)
      (print sum))))

(defun hdf-table-read-test ()
  (with-open-hdf-file (file "/home/ghollisjr/hdf.h5"
                            :direction :input)
    (let ((table
           (open-hdf-table file "/test"))
          (sum 0))
      (do-table (row-index table)
          ("x")
        (incf sum x))
      (table-close table)
      (print sum))))

(defun individual-test ()
  (with-open-hdf-file (file "/home/ghollisjr/modular.h5"
                            :direction :input)
    (let ((table (open-hdf-table file "/test/tables/x0"))
          (sum 0))
      (do-table (row-index table)
          ("x0")
        (incf sum x0))
      (table-close table)
      (print sum))))

(defun individual-test2 ()
  (with-open-hdf-file (file "/home/ghollisjr/modular.h5"
                            :direction :input)
    (let ((x0-table (open-hdf-table file "/test/tables/x0"))
          (x1-table (open-hdf-table file "/test/tables/x1"))
          (sum 0))
      (dotimes (i (table-nrows x0-table))
        (table-load-next-row x0-table)
        (table-load-next-row x1-table)
        (incf sum (table-get-field x0-table :x0)))
      (table-close x0-table)
      (table-close x1-table)
      (print sum))))
