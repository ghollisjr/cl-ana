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

(in-package :cl-ana.makeres-macro)

(declaim (optimize (debug 3)))

;;; Macros

(defvar *proj->res-macros*
  (make-hash-table :test 'equal)
  "Map from project to macro symbols for project")

(defmacro define-res-macro (name lambda-list &body body)
  "Defines a res-macro to be expanded in the pipeline."
  `(progn
     (symbol-macrolet ((macros (gethash (project) *proj->res-macros*)))
       (setf macros
             (adjoin ',name macros
                     :test #'eq)))
     (defmacro ,name ,lambda-list
       ,@body)))

(defparameter *cl-let-ops*
  (list 'let
        'let*
        'symbol-macrolet)
  "list of operators following the let binding format")

(defparameter *cl-flet-ops*
  (list 'flet
        'labels
        'macrolet)
  "list of operators following the flet binding format")

(defparameter *cl-bind-ops*
  (list 'destructuring-bind
        'multiple-value-bind)
  "list of operators following the *-bind binding format")

(defparameter *lambda-ops*
  (list 'lambda
        ;; from cl-ana:
        'mlambda
        'klambda))

(defparameter *lambda-expander*
  (lambda (expander form)
    (destructuring-bind (op lambda-list &rest body) form
      (let* ((lambda-list
              (mapcar (lambda (v)
                        (if (listp v)
                            (list* (first v)
                                   (funcall expander (second v))
                                   (rest (rest v)))
                            v))
                      lambda-list))
             (body (mapcar expander body)))
        (list* op lambda-list body)))))

(defparameter *cl-let-expander*
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
  "Expander function for common lisp let-like operators needing
  special treatment during expansion.")

(defparameter *cl-flet-expander*
  (lambda (expander form)
    (destructuring-bind (op bindings &rest body) form
      (let* ((fnames (mapcar #'first bindings))
             (fargs (mapcar #'second bindings))
             (fbodies
              (mapcar (lambda (x) (mapcar expander (rest (rest x))))
                      bindings))
             (fargs-expanded
              (mapcar (lambda (lambda-list)
                        (mapcar (lambda (x)
                                  (if (listp x)
                                      (list* (first x)
                                             (funcall expander (second x))
                                             (when (third x)
                                               (list (third x))))
                                      x))
                                lambda-list))
                      fargs))
             (body (mapcar expander
                           body)))
        (list* op (mapcar #'list*
                          fnames fargs-expanded fbodies)
               body))))
  "Expander function for common lisp flet-like operators needing
  special treatment during expansion.")

(defparameter *cl-bind-expander*
  (lambda (expander form)
    (destructuring-bind (op bind-form expr &rest body) form
      (let* ((expr (funcall expander expr))
             (body (mapcar expander
                           body)))
        (list* op
               bind-form
               expr
               body))))
  "Expander function for common lisp *-bind operators needing special
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
              (append *cl-let-ops*
                      *cl-flet-ops*
                      *cl-bind-ops*
                      *lambda-ops*))))))

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
       for op in *cl-let-ops*
       do (setf (gethash op op->expander)
                *cl-let-expander*))
    (loop
       for op in *cl-flet-ops*
       do (setf (gethash op op->expander)
                *cl-flet-expander*))
    (loop
       for op in *cl-bind-ops*
       do (setf (gethash op op->expander)
                *cl-bind-expander*))
    (loop
       for op in *lambda-ops*
       do (setf (gethash op op->expander)
                *lambda-expander*))))

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
    (labels ((rec (f &optional no-op)
               ;; non-nil no-op means that the form being passed
               ;; should not be treated for operator content.
               (let ((result
                      (cond
                        ((atom f)
                         f)
                        ((and (not no-op)
                              (member (first f) resmacs :test #'eq))
                         (let ((newf (macroexpand-1 f)))
                           (rec newf)))
                        ((and (not no-op)
                              (member (first f)
                                      binding-ops :test #'eq))
                         (let* ((op (first f))
                                (expander
                                 (gethash op
                                          (gethash (project)
                                                   *proj->op->expander*))))
                           (funcall expander #'rec f)))
                        (t
                         (cons (rec (car f))
                               (rec (cdr f) t))))))
                 result)))
      (rec expr))))

;;;; Functions
(defmacro define-res-function (name lambda-list &body body)
  "Defines a res-macro with the same syntax you would use for defining
a function, which is just removing the double-evaluation of the body
form.  Note that this still just defines a macro, so wrap calls to it
in a lambda form when e.g. mapping."
  `(define-res-macro ,name (&rest args)
     `(destructuring-bind ,',lambda-list (list ,@args)
        ,@',body)))

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

(defpropogator #'macrotrans
    (lambda (graph)
      (macrotrans graph)))
