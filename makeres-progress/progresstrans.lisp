;;;; makeres-progress is a Common Lisp make-like tool for computations.
;;;; Copyright 2014 Gary Hollis
;;;;
;;;; This file is part of makeres-progress.
;;;;
;;;; makeres-progress is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; makeres-progress is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-progress.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.makeres-progress)

(defparameter *makeres-progress-results-only-p* t
  "Set to nil if you want progresstrans to print messages for every
target in given table, not just the ones also present in the project
target table (meaning that internal-use-only targets will have their
strange names printed).")

(defun progresstrans (target-table)
  "Adds status printing message to any targets present in both
target-table and the project target table unless
*makeres-progress-results-only-p* is nil.  See
*makeres-progress-results-only-p* for more details."
  (let ((result-table (make-hash-table :test 'equal))
        (progress-ids
         (if *makeres-progress-results-only-p*
             (loop
                for id being the hash-keys in (target-table)
                collecting id)
             ;; subtle difference, read carefully
             (loop
                for id being the hash-keys in target-table
                collecting id))))
    (loop
       for id being the hash-keys in target-table
       do
         (if (member id progress-ids :test #'equal)
             (let* ((target (gethash id target-table))
                    (expr (target-expr target)))
               (setf (gethash id result-table)
                     (make-target id
                                  `(progn
                                     (let ((*print-pretty* nil))
                                       (format t "Computing ~s~%"
                                               ',id))
                                     ,expr)
                                  :val (target-val target)
                                  :stat (target-stat target)
                                  :timestamp (target-timestamp target))))
             (setf (gethash id result-table)
                   (copy-target (gethash id target-table)))))
    result-table))
