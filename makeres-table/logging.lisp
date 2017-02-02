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
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.makeres-table)

;; Default behavior for tables: do nothing, they're usually stored via
;; files which should go in the work/ directory.

;;;; So far, the cleanest approach to handling non-tab-generated
;;;; tables is to provide operators for source tables.  This way,
;;;; save-target and load-target could inspect the form to determine
;;;; how to re-open a table once it's been closed by save-target or
;;;; cleanup.
;;;;
;;;; Alternatives:
;;;;
;;;; source and bootstrap operators are not technically needed, as
;;;; tables which do not originate from a tab reduction could simply
;;;; have their expressions reevaluated to re-open them after they are
;;;; closed by save-object.  This would carry the penalty of breaking
;;;; future operators however, if they were to do fancy things inside
;;;; the table creation form.
;;;;
;;;; Or, save-target when called on a non-tab-generated table could
;;;; unset the target-stat for the source table, meaning that
;;;; subsequent calls to makeres would be required to access the
;;;; source table.  This would carry the penalty of not having
;;;; guaranteed access to previously computed results however.
;;;;
;;;; Also, since this functionality needs to be built into
;;;; save-target, this file actually needs to be moved to
;;;; makeres-table to avoid circular dependencies.  logres-table will
;;;; then be absorbed into makeres-table.

;; (define-load-target-method table id
;;     (or (cl-ana.makeres-table::tab?
;;          (target-expr (gethash id (target-table))))
;;         (cl-ana.makeres-table::srctab?
;;          (target-expr (gethash id (target-table)))))
;;   (let* ((tar (gethash id (target-table)))
;;          (expr (target-expr tar))
;;          (opener
;;           (eval
;;            (if (cl-ana.makeres-table::srctab?
;;                 (target-expr (gethash id (target-table))))
;;                (destructuring-bind (progn (srctab opener
;;                                                   &optional bootstrap)) expr
;;                  opener)
;;                (destructuring-bind (progn
;;                                      (tab source inits opener &rest body))
;;                    expr
;;                  opener)))))
;;     (setf (target-val tar)
;;           (funcall opener :read))))

;; (defmethod save-object ((tab table) path)
;;   (table-close tab))

;; (defmethod load-object ((type (eql 'table)) path)
;;   nil)

;; (defmethod destruct-on-save? ((tab table))
;;   t)

;; (defmethod cleanup ((tab table))
;;   (table-close tab))

;; reusable-table technically not a table:

;; (define-load-target-method reusable-table id
;;     (or (cl-ana.makeres-table::tab?
;;          (target-expr (gethash id (target-table))))
;;         (cl-ana.makeres-table::srctab?
;;          (target-expr (gethash id (target-table)))))
;;   (let* ((tar (gethash id (target-table)))
;;          (expr (target-expr tar))
;;          (opener
;;           (eval
;;            (if (cl-ana.makeres-table::srctab?
;;                 (target-expr (gethash id (target-table))))
;;                (destructuring-bind (progn (srctab opener
;;                                                   &optional bootstrap)) expr
;;                  opener)
;;                (destructuring-bind (progn
;;                                      (tab source inits opener &rest body))
;;                    expr
;;                  opener)))))
;;     (setf (target-val tar)
;;           (funcall opener :read))))

;; Old versions:
;; (defmethod save-object ((tab reusable-table) path)
;;   (table-close tab))

;; (defmethod load-object ((type (eql 'reusable-table)) path)
;;   nil)

;; New versions:

(defmethod printable ((tab reusable-table))
  nil)

(defmethod save-object ((tab reusable-table) path)
  ;; Write opener form to file:
  (with-open-file (file path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (let ((opener-form
           (reusable-table-opener-form tab)))
      (format file "~s~%" opener-form)))
  (table-close tab))

(defmethod load-object ((type (eql 'reusable-table)) path)
  (let* ((opener-form
         (with-open-file (file path
                               :direction :input)
           (read file)))
         (opener (eval opener-form)))
    (funcall opener :read)))

(defmethod destruct-on-save? ((tab reusable-table))
  t)

(defmethod cleanup ((tab reusable-table))
  (table-close tab))
