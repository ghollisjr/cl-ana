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
;;;; along with makeres.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.makeres-branch)

(defmacro branch (branch-list form &optional (test 'eql))
  "branch operator takes the branch-list and execution body as arguents.

branch-list must be either 1. A form which can be evaluated in the
  null-environment to yield a list whose elements each correspond to a
  separate branch of a computation, or 2. A (res id) form which
  denotes co-branching.  Each branching computation has access to the
  value during its evaluation, and branch operators can be nested,
  although this is technically unnecessary."
  nil)

;; These branch functions take full target expressions
(defun branch? (expr)
  (destructuring-bind (progn &rest forms)
      expr
    (and (single forms)
         (eq (first (first forms)) 'branch))))

(defun branch-list (expr)
  (when (branch? expr)
    (second (second expr))))

(defun branch-expr (expr)
  (when (branch? expr)
    (third (second expr))))

(defun branch-test (expr)
  (let ((result (fourth (second expr))))
    (if result
        result
        'eql)))

(defun res? (expr)
  (and (listp expr)
       (eq (first expr) 'res)))

(defun unres (expr)
  (second expr))

(defun branch-chains (graph)
  "Finds chains of branches; returns list of id chains, each of the
form (source &rest co-branches)"
  (let* ((branch-ids
          (loop
             for id being the hash-keys in graph
             for tar being the hash-values in graph
             when (let ((expr (target-expr tar)))
                    (branch? expr))
             collecting id))
         (source-branch-ids
          (loop
             for id in branch-ids
             when (let ((expr (target-expr (gethash id graph))))
                    (not (res? (branch-list expr))))
             collecting id))
         ;; map from id to all immediate dependents
         (id->imm-dependent
          (let ((result (make-hash-table :test 'equal)))
            (loop
               for id in branch-ids
               when (res? (branch-list
                           (target-expr
                            (gethash id graph))))
               do (let ((source (unres
                                 (branch-list
                                  (target-expr
                                   (gethash id graph))))))
                    (setf (gethash source
                                   result)
                          (adjoin id (gethash source result) :test #'equal))))
            result)))
    (labels ((collect-dependents (id)
               ;; returns list of all dependents on id
               (append (gethash id id->imm-dependent)
                       (mapcan #'collect-dependents
                               (gethash id id->imm-dependent)))))
      (loop
         for source in source-branch-ids
         collecting (cons source
                          (collect-dependents source))))))


;;;; Branch transformation algorithm:
;;;;
;;;; The algorithm is recursive, applying a single pass to every layer
;;;; of nested branches present in the target table.  It assumes that
;;;; efficient recombination of targets into passes will be done via
;;;; other graph transformations occurring later in the transformation
;;;; pipeline, e.g. makeres-table.
;;;;
;;;; 1. Find branch chains, branches with shared sources
;;;;
;;;; 2. Create targets for each individual branch.
;;;;
;;;; 3. Modify targets of final result targets to simply collect
;;;;    hash-tables of the resulting passes

(defun branchtrans (graph)
  (let* ((result
          (cl-ana.makeres:copy-target-table graph))
         (branch-chains
          (branch-chains graph))
         (source-branch-ids (mapcar #'first branch-chains))
         (source->branch-list
          (let ((id->list (make-hash-table :test 'equal)))
            (loop
               for id in source-branch-ids
               do (setf (gethash id id->list)
                        (eval
                         (branch-list
                          (target-expr
                           (gethash id graph))))))
            id->list)))
    (if branch-chains
        (progn
          (loop
             for chain in branch-chains
             do (let* ((source (first chain))
                       (branch-list
                        (gethash source source->branch-list))
                       (id->branch->gsym
                        (let ((ht (make-hash-table :test 'equal)))
                          (loop
                             for id in chain
                             do
                               (let* ((raw-expr
                                       (target-expr
                                        (gethash id graph)))
                                      (test (branch-test raw-expr)))
                                 (setf (gethash id ht)
                                       (make-hash-table :test test)))
                               (loop
                                  for branch in branch-list
                                  do (setf (gethash branch
                                                    (gethash id ht))
                                           (gensym))))
                          ht)))
                  (loop
                     for id in chain
                     do (let* ((raw-expr
                                (target-expr
                                 (gethash id graph)))
                               (test (branch-test raw-expr))
                               (expr
                                (let ((e (branch-expr raw-expr)))
                                  (if (eq (first e) 'progn)
                                      e
                                      `(progn ,e)))))
                          (loop
                             for branch in branch-list
                             do (let ((gsym
                                       (gethash branch
                                                (gethash id
                                                         id->branch->gsym)))
                                      (body
                                       (sublis
                                        (append (list (cons '(branch)
                                                            `',branch))
                                                (loop
                                                   for chain-id in chain
                                                   collecting
                                                     (cons
                                                      `(res ,chain-id)
                                                      `(res ,(gethash branch
                                                                      (gethash chain-id
                                                                               id->branch->gsym))))))
                                        expr
                                        :test #'equal)))
                                  (setf (gethash gsym result)
                                        (make-target gsym
                                                     body))))
                          (setf (target-expr (gethash id result))
                                `(let ((result (make-hash-table :test ',test)))
                                   ,@(loop
                                        for branch in branch-list
                                        collecting
                                          `(setf (gethash ',branch result)
                                                 (res ,(gethash branch
                                                                (gethash id id->branch->gsym)))))
                                   result))))))
          (branchtrans result))
        result)))
