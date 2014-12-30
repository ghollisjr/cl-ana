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

(in-package :cl-ana.makeres)

;;;; This file serves as a regrettably bad example as to how to write
;;;; target table/graph transformations.
;;;;
;;;; The following are rules for graph/target table transformations:
;;;;
;;;; * The graph given as an argument is a free to be modified in any
;;;;   way, but only the return value is used for the next
;;;;   transformation in the pipeline.
;;;;
;;;; * Modification of the global variables in the makeres package can
;;;;   be done at the user's peril, this is highly dependent on the
;;;;   order in which information is pulled/pushed into these
;;;;   variables.  The lrestrans is an example of how to do this (and
;;;;   how messy it is).
;;;;
;;;; * All targets present in (gethash *project-id* *target-tables*)
;;;;   must be present in the output of the last transformation in the
;;;;   pipeline; no need to restrict these too much by forcing it on
;;;;   every stage.  You the user can decide whether you want to be a
;;;;   hard-ass.
;;;;
;;;; * The output of the last transformation in the pipeline goes in
;;;;   (gethash *project-id* *fin-target-tables*), so modifying this
;;;;   table is probably a bad idea for your algorithm.
;;;;
;;;; * Target dependencies created via make-target make use of
;;;;   find-dependencies, so if you want a different mechanism you
;;;;   need to use (make-instance 'target ...) and supply explicit
;;;;   values when you create the output graph.

;; lrestrans uses a new operator, lres, which denotes a logical result
;; to be recomputed every time makeres is run.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Note that this method is dirty; the target-stat values are
  ;; assumed to be unmanaged by the transformations due to the model
  ;; of computation: they are to return a new graph without knowledge
  ;; of previous versions etc.  This transformation directly violates
  ;; this assumption.
  (defun lrestrans (target-table)
    "Sets all lres result statuses in the final target table and
target table to nil"
    (let ((result (make-hash-table :test 'equal)))
      (loop
         for key being the hash-keys in target-table
         for val being the hash-values in target-table
         do (progn
              (setf (gethash key result)
                    (copy-target val))
              (when (eq (first (second (target-expr val)))
                        'lres)
                (destructuring-bind (progn
                                      expr
                                      &rest
                                      someone-does-not-understand-lres)
                    (target-expr val)
                  (destructuring-bind (lres expr) expr
                    ;; immediate result table:
                    (setf (target-stat (gethash key
                                                result))
                          nil)
                    (setf (target-expr (gethash key
                                                result))
                          `(progn ,expr))
                    (unsetresfn key))))))
      result)))
