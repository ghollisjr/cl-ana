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

(defmacro branch (branch-list &body body)
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

(defun branch-body (expr)
  (when (branch? expr)
    (rest (rest (second expr)))))

(defun res? (expr)
  (and (listp expr)
       (eq (first expr) 'res)))

(defun branchtrans (graph)
  (let* ((result
          (cl-ana.makeres:copy-target-table graph))
         (branch-ids
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
             collecting id)))
    nil))
