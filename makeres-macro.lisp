;;;; makeres-macro is a Common Lisp make-like tool for computations.
;;;; Copyright 2014 Gary Hollis
;;;; 
;;;; This file is part of makeres-macro.
;;;; 
;;;; makeres-macro is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; makeres-macro is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-macro.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(in-package makeres-macro)

;; uncomment when finished:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *proj->res-macros*
    (make-hash-table :test 'equal)
    "Map from project to macro symbols for project")

  (defmacro define-res-macro (name lambda-list &rest body)
    "Defines a res-macro to be expanded in the pipeline."
    (symbol-macrolet ((macros (gethash (project) *proj->res-macros*)))
      (setf macros
            (adjoin name macros
                    :test #'eq))
      `(defmacro ,name ,lambda-list
         ,@body)))

  ;; res-macro expansion rule: inner-most macros are expanded first,
  ;; first macros found expanded before later macros
  (defun expand-res-macros (expr)
    "Finds & repeatedly expands any res-macros present in expr until
none are present."
    (let ((resmacs (gethash (project) *proj->res-macros*)))
      (labels ((rec (f)
                 (let ((result
                        (cond
                          ((atom f)
                           f)
                          ((member (first f) resmacs :test #'eq)
                           (let ((newf (macroexpand-1 f)))
                             (rec newf)))
                          (t
                           (mapcar #'rec f)))))
                   result)))
        (rec expr))))

  (defun macrotrans (target-table)
    "Implements macro expansion transformation"
    (let ((result
           (copy-target-table target-table)))
      (loop
         for tar being the hash-values in result
         do (setf (target-expr tar)
                  (expand-res-macros (target-expr tar))))
      result)))
