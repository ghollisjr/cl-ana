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

(declaim (optimize (debug 3)))

(defvar *proj->res-macros*
  (make-hash-table :test 'equal)
  "Map from project to macro symbols for project")

(defmacro define-res-macro (name lambda-list &rest body)
  "Defines a res-macro to be expanded in the pipeline."
  `(progn
     (symbol-macrolet ((macros (gethash (project) *proj->res-macros*)))
       (setf macros
             (adjoin ',name macros
                     :test #'eq)))
     (defmacro ,name ,lambda-list
       ,@body)))

(defparameter *cl-binding-ops*
  (list 'let
        'let*
        'flet
        'labels
        'macrolet
        'symbol-macrolet)
  "list of operators employing bindings from plain common lisp")

(defparameter *cl-expander*
  (lambda (expander form)
    (destructuring-bind (op bindings &rest body) form
      (let* ((bsyms (mapcar #'first bindings))
             (bexprs
              (mapcar expander
                      (mapcar #'second bindings)))
             (body (mapcar expander
                           body)))
        (list* op
               (mapcar (lambda (sym bind)
                         (list sym bind))
                       bsyms bexprs)
               body))))
  "Expander function for common lisp operators needing special
  treatment during expansion.")

(defvar *proj->binding-ops*
  (make-hash-table :test 'equal))

;; Since each operator may have a different structure/different areas
;; off-limits to expansion, this map stores the expander functions
;; (taking expander function and form arguments)
(defvar *proj->op->expander*
  (make-hash-table :test 'equal)
  "map from project to operator to expander function")

(defun ensure-binding-ops ()
  (symbol-macrolet ((binding-ops
                     (gethash (project) *proj->binding-ops*)))
    (let ((stat
           (second
            (multiple-value-list binding-ops))))
      (when (not stat)
        (setf binding-ops
              *cl-binding-ops*)))))

(defun ensure-op-expanders ()
  (symbol-macrolet ((op->expander
                     (gethash (project)
                              *proj->op->expander*)))
    (let ((stat
           (second
            (multiple-value-list op->expander))))
      (when (not stat)
        (setf op->expander
              (make-hash-table :test 'eq))))
    (loop
       for op in *cl-binding-ops*
       do (setf (gethash op op->expander)
                *cl-expander*))))

(defun add-binding-ops (ops-expanders)
  "Takes a list of lists of the form (op expander) and adds the
operators along with their expanders to the current project."
  (ensure-binding-ops)
  (ensure-op-expanders)
  (symbol-macrolet ((binding-ops
                     (gethash (project)
                              *proj->binding-ops*))
                    (op->expander
                     (gethash (project)
                              *proj->op->expander*)))
    (let ((ops (cars ops-expanders)))
      (setf binding-ops
            (list->set (append ops binding-ops)
                       #'equal)))
    (loop
       for (op expander) in ops-expanders
       do (setf (gethash op op->expander)
                expander))))

;; res-macro expansion rule: inner-most macros are expanded first,
;; first macros found expanded before later macros
(defun expand-res-macros (expr)
  "Finds & repeatedly expands any res-macros present in expr until
none are present."
  (let ((resmacs (gethash (project) *proj->res-macros*))
        (binding-ops (gethash (project) *proj->binding-ops*)))
    (labels ((rec (f)
               (let ((result
                      (cond
                        ((atom f)
                         f)
                        ((member (first f) resmacs :test #'eq)
                         (let ((newf (macroexpand-1 f)))
                           (rec newf)))
                        ((member (first f)
                                 binding-ops :test #'eq)
                         (let* ((op (first f))
                                (expander
                                 (gethash op
                                          (gethash (project)
                                                   *proj->op->expander*))))
                           (funcall expander #'rec f)))
                        (t
                         (mapcar #'rec f)))))
                 result)))
      (rec expr))))

(defun macrotrans (target-table)
  "Implements macro expansion transformation"
  ;; initialize *proj->binding-ops* for project if necessary:
  (ensure-binding-ops)
  (ensure-op-expanders)
  (let ((result
         (copy-target-table target-table)))
    (loop
       for id being the hash-keys in result
       for tar being the hash-values in result
       do (let* ((newtar (copy-target tar))
                 (oldval (target-val tar))
                 (oldstat (target-stat tar)))
            (setf (gethash id result)
                  (make-target id
                               (expand-res-macros (target-expr tar))
                               :val oldval
                               :stat oldstat))))
    result))
