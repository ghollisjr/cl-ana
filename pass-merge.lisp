(in-package :makeres-tabletrans)

;;; New operator: table-pass, which is used to denote passing over a
;;; table (cl-ana table).  Define the operator as
;;;
;;; (table-pass table fields inits return &body body)
;;;
;;; which places each binding in inits, executes body once per row
;;; using field selections from fields, and finally returning the
;;; return form outside the loop but inside the let expression.
;;;
;;; Rules:
;;;
;;; 1. Immediate dependents of the table which do not depend on
;;;    information from prior passes still needing acquisition should
;;;    be executed in parallel in a single pass over the table.
;;;
;;; 2. inits from these parallel targets should be merged, and any
;;;    common bindings only occur once.
;;;
;;; 3. As for any transformation, the resulting table must contain all
;;;    the initial targets.
;;;
;;; Approach is to create parallelized targets which return lists of
;;; all needed table-pass results.  The final targets are added as
;;; dependencies of these returned lists.  All other targets are left
;;; unchanged.  Status and values are copied from input target-table
;;; for applicable targets, and previous statuses and values for
;;; returned table are read if available.

;;; Let's define the transformation:

(defmacro expand (form &environment env)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand form env)
    `(values ',expansion ',expanded-p)))

;; for ease of editor use, define a macro:
(defmacro table-pass (table inits result &body body)
  "Loops over table with external bindings inits and result form
result, executing body once per row.

macro field yields the field value of current row.

macro row-number yields the row number of current row.

Limitations: Make sure no forms (field X) occur which are not meant to
reference the field value.  I've tried various options to make this
work via macros but nothing short of code walking looks viable, and
this is my naive code walking strategy's fault."
  ;; local macro fields will accept either symbol or string, and will
  ;; convert a symbol into a lower-case string for use in fields.

  ;; Having difficulties expanding the body to get the fields which
  ;; are present, trying to use &environment with expand macro but not
  ;; much luck so far.
  (alexandria:with-gensyms (ri)
    `(macrolet ((field (field-sym)
                  (intern (lispify field-sym))))
       (let ,inits
         (do-table (,ri ,table)
             ,(list->set (mapcar (alexandria:compose #'intern
                                                     #'lispify)
                                 (makeres::find-dependencies body 'field))
                         #'string=)
           ,@(remove-if-not (lambda (x)
                              (and (listp x)
                                   (eq (first x)
                                       'declare)))
                            body)
           (flet ((row-number ()
                    ,ri))
             ,@(remove-if (lambda (x)
                            (and (listp x)
                                 (eq (first x)
                                     'declare)))
                          body)))
         ,result))))

;; must be available at compile and load times:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tabletrans-symmap*
    (make-hash-table :test 'equal))

  ;; need to makesure that pass targets have appropriate statuses
  ;; based on dependencies of table-pass targets; makeres now sets
  ;; statuses based on supplied parameters in original target table.
  (defun tabletrans (target-table)
    "Transforms target-table according to table-pass operator."
    (let* (;; result
           (result-table (make-hash-table :test 'equal))
           ;; all table-pass targets:
           (table-pass-ids nil)
           ;; map from id to table dependency
           (id->tab (make-hash-table :test 'equal))
           ;; map from id to expr
           (id->body (make-hash-table :test 'equal))
           ;; map from id to init bindings
           (id->inits (make-hash-table :test 'equal))
           ;; map from id to return
           (id->return (make-hash-table :test 'equal))
           ;; map from table to immediate dependencies:
           (tab->ids (make-hash-table :test 'equal)))
      (flet ((group-ids-by-pass (tab)
               ;; Returns list of id lists grouped by pass which should
               ;; be generated from table
               (let* ((ids (gethash tab tab->ids))
                      (dep< (dep< target-table))
                      (sorted-ids
                       (sort (copy-list ids)
                             dep<)))
                 (let ((result nil)
                       (pass (list (first sorted-ids)))
                       (lst (rest sorted-ids)))
                   (labels ((rec ()
                              ;; Algorithm is linear in number of
                              ;; passes required to compute final
                              ;; results
                              (dolist (i lst)
                                (when (every (lambda (p)
                                               (funcall dep< i p))
                                             pass)
                                  (push i pass)))
                              (push (nreverse pass) result)
                              (setf lst
                                    (remove-if (lambda (l)
                                                 (member l pass
                                                         :test #'equal))
                                               lst))
                              (if lst
                                  (progn
                                    (setf pass (list (first lst)))
                                    (setf lst (rest lst))
                                    (rec))
                                  (progn
                                    (nreverse result)))))
                     (rec))))))
        ;; Must handle progn in table-expr, always present for
        ;; ease of use of defres
        (loop
           for id being the hash-keys in target-table
           for tar being the hash-values in target-table
           when (and (second (target-expr tar))
                     (listp (second (target-expr tar)))
                     (eq (first (second (target-expr tar)))
                         'table-pass))
           do
             (destructuring-bind
                   (progn
                     (table-pass table inits return &body body))
                 (target-expr tar)
               (push id table-pass-ids)
               (setf (gethash id id->tab)
                     table)
               (setf (gethash id id->body)
                     `(progn ,@body))
               (setf (gethash id id->inits)
                     inits)
               (setf (gethash id id->return)
                     return)
               (push id
                     (gethash table tab->ids))))
        ;; Create new targets and rebind old ones:
        (loop
           for tab being the hash-keys in tab->ids
           for ids being the hash-values in tab->ids
           do
             (let (;; list of id lists per pass.
                   (ids-by-pass
                    (group-ids-by-pass tab)))
               ;; do stuff with ids by pass
               (loop
                  for pass in ids-by-pass
                  for pass-number from 1
                  do
                    (flet ((merge-bindings (ids)
                             (mapcan (lambda (id)
                                       (copy-list (gethash id id->inits)))
                                     ids)))
                      ;; compute expressions
                      (let* (;; only keep pass ids which need updating
                             (pass-need (remove-if
                                         (lambda (id)
                                           (target-stat
                                            (gethash id target-table)))
                                         pass))
                             (bindings
                              (merge-bindings pass-need))
                             (expr
                              (when pass-need
                                `(table-pass (wrap-for-reuse ,tab)
                                     ,bindings
                                     (list ,@(loop
                                                for id in pass-need
                                                collecting
                                                  (gethash id
                                                           id->return)))
                                   ,@(loop
                                        for id in pass-need
                                        collecting (gethash id id->body)))))
                             (id-key (list tab
                                           `(pass ,pass-number))))
                        ;; create pass target
                        (setf (gethash (aif
                                        (gethash id-key
                                                 *tabletrans-symmap*)
                                        it
                                        ;; set symbol in symmap & return:
                                        (setf (gethash id-key
                                                       *tabletrans-symmap*)
                                              (gensym)))
                                       result-table)
                              (make-target (gethash id-key
                                                    *tabletrans-symmap*)
                                           expr))
                        ;; Set pass target status based on immediate
                        ;; dependents
                        (setf
                         (target-stat
                          (gethash (gethash id-key
                                            *tabletrans-symmap*)
                                   result-table))
                         (every (lambda (p)
                                  (target-stat
                                   (gethash p target-table)))
                                pass))

                        ;; create new targets for final results:
                        (let ((tabsym
                               (gethash id-key
                                        *tabletrans-symmap*)))
                          ;; need executing
                          (loop
                             for id in pass-need
                             for index from 0
                             do (setf
                                 (gethash id result-table)
                                 ;; copy old value and status
                                 (let* ((oldtar
                                         (gethash id target-table))
                                        (old-val
                                         (target-val oldtar))
                                        (old-stat
                                         (target-stat oldtar)))
                                   (make-target id
                                                `(elt (res ,tabsym) ,index)
                                                :val old-val
                                                :stat old-stat))))
                          ;; don't need executing
                          (loop
                             for id in (set-difference pass pass-need
                                                       :test #'equal)
                             for index from 0
                             do (setf
                                 (gethash id result-table)
                                 ;; copy old value and status
                                 (let* ((oldtar
                                         (gethash id target-table))
                                        (old-val
                                         (target-val oldtar))
                                        (old-stat
                                         (target-stat oldtar)))
                                   (make-target id
                                                `(elt (res ,tabsym) ,index)
                                                :val old-val
                                                :stat old-stat))))))))))
        ;; Copy old bindings:
        (loop
           for id being the hash-keys in target-table
           when (not (member id table-pass-ids
                             :test #'equal))
           do (setf (gethash id result-table)
                    (gethash id target-table)))
        result-table))))
