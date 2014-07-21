(in-package :makeres-tabletrans)

;;; New operator: dotab, which is used to denote passing over a
;;; table (cl-ana table).  Define the operator as
;;;
;;; (dotab table inits return &body body)
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

;;; Must use one file per output table methodology since hdf5 datasets
;;; can't be modified (I think so, it didn't work in the past if I
;;; remember correctly) since some tables sharing a file may need to
;;; be written and read simulaneously.
;;;
;;; This frees up a possibility for file management: done inside table
;;; target expressions.  One of the table-pass bindings would be the
;;; file holding the table, and depending on the target status it
;;; could determine whether to open the file for reading or writing.
;;;
;;; The table return expression could be to first close the file, then
;;; re-open the file for reading.  *** Just found: h5fclose sets the
;;; file to be closed once all accessed contents are closed, so I can
;;; immediately close the file after getting the table object from
;;; inside, freeing the need to manage the file!

(defmacro dotab (source-table init-bindings return &body body)
  "Operator used for denoting a loop over a table.

init-bindings are placed in a let* outside the loop body, which
executes body once per row in a context where the macro field is
defined which has access to any physical or logical fields by their
symbol.

return is the return value of the dotab, executed inside the
init-bindings let form."
  `(table-pass ,source-table ,init-bindings ,return () ,@body))

;; must be available at compile and load times:
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; need to makesure that pass targets have appropriate statuses
  ;; based on dependencies of table-pass targets; makeres now sets
  ;; statuses based on supplied parameters in original target table.
  (defun pass-merge (target-table)
    "Transforms target-table according to table-pass operator."
    ;; Clear gsym table:
    (clrgsym 'pass-merge)
    ;; Initialization: Expand all dotab targets into table-pass for merging
    (loop
       for id being the hash-keys in target-table
       for tar being the hash-values in target-table
       do (let ((expr (target-expr tar)))
            (when (and expr
                       (listp expr))
              (destructuring-bind (progn &rest body) (target-expr tar)
                (when (eq (first (first body))
                          'dotab)
                  (let ((copy (copy-target tar)))
                    (setf (target-expr copy)
                          (list 'progn
                                (macroexpand-1 (first body))))
                    (setf (gethash id target-table)
                          copy)))))))
    (let* (;; result
           (result-table (make-hash-table :test 'equal))
           ;; all table-pass targets:
           (table-pass-ids nil)
           ;; map from id to table dependency
           (id->tab (make-hash-table :test 'equal))
           ;; map from id to deps
           (id->deps (make-hash-table :test 'equal))
           ;; map from id to pdeps:
           (id->pdeps (make-hash-table :test 'equal))
           ;; map from id to expr
           (id->body (make-hash-table :test 'equal))
           ;; map from id to init bindings
           (id->inits (make-hash-table :test 'equal))
           ;; map from id to lfields:
           (id->lfields (make-hash-table :test 'equal))
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
                              (push (reverse pass) result)
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
           when (destructuring-bind (progn &rest body)
                    (target-expr tar)
                  (and body
                       (listp (first body))
                       (eq (first (first body))
                           'table-pass)))
           do
             (destructuring-bind
                   (progn
                     (table-pass table inits return lfields &body body))
                 (target-expr tar)
               (push id table-pass-ids)
               (setf (gethash id id->tab)
                     table)
               (setf (gethash id id->deps)
                     (target-deps tar))
               (setf (gethash id id->pdeps)
                     (target-pdeps tar))
               (setf (gethash id id->body)
                     `(progn ,@body))
               (setf (gethash id id->inits)
                     inits)
               (setf (gethash id id->lfields)
                     lfields)
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
                    (let* ((pass-need (remove-if
                                       (lambda (id)
                                         (target-stat
                                          (gethash id target-table)))
                                       pass))
                           (pass->initsymmap
                            (let ((result (make-hash-table :test 'equal)))
                              (loop for p in pass-need
                                 do (setf (gethash p result)
                                          (destructuring-bind
                                                (progn-op
                                                 (tab-op src inits &rest xs))
                                              (target-expr
                                               (gethash p target-table))
                                            (loop
                                               for b in inits
                                               collect (list (first b)
                                                             (gsym 'pass-merge))))))
                              result)))
                      (flet ((merge-inits (ids symmap)
                               (loop
                                  for p in ids
                                  append
                                    (loop
                                       for b in (gethash p id->inits)
                                       for gsymb in (gethash p symmap)
                                       collect `(,(second gsymb)
                                                  (symbol-macrolet
                                                      ,(gethash p pass->initsymmap)
                                                    ,@(rest b))))))
                             (merge-lfields (src ids symmap)
                               (list->set
                                (append
                                 (when (and (listp src)
                                            (eq (first src) 'res))
                                   (append
                                    (awhen (gethash *project-id* *proj->tab->lfields*)
                                      (gethash (second src)
                                               it))
                                    (awhen (gethash *project-id*
                                                    *proj->tab->ltab-lfields*)
                                      (gethash (second src)
                                               it))))
                                 (mapcan
                                  (lambda (id)
                                    (let ((oldbindings
                                           (gethash id id->lfields)))
                                      (mapcan
                                       (lambda (old)
                                         (list
                                          `(,(gethash (first old)
                                                      (gethash id symmap))
                                             ,@(rest old))))
                                       oldbindings)))
                                  ids)))))
                        ;; compute expressions
                        (let* (;; only keep pass ids which need updating

                               ;; bindings needs updating due to using
                               ;; gensyms for init bindings
                               (inits
                                (merge-inits pass-need pass->initsymmap))
                               (pass->lfieldsymmap
                                (let (;; map from pass id to hash table
                                      ;; mapping from first symbol from
                                      ;; raw lfield binding to gensym
                                      (result
                                       (make-hash-table :test 'equal)))
                                  (loop
                                     for p in pass-need
                                     do (setf
                                         (gethash p result)
                                         (let ((map (make-hash-table :test 'equal)))
                                           (destructuring-bind
                                                 (progn-op
                                                  (tab-op src inits r lfields &rest xs))
                                               (target-expr (gethash p target-table))
                                             (loop
                                                for i in lfields
                                                do (setf (gethash (first i) map)
                                                         (gsym 'pass-merge))))
                                           map)))
                                  result))
                               (lfield-bindings
                                ;; lfields come from all targets, not
                                ;; just the ones which need passing.
                                (merge-lfields tab pass pass->lfieldsymmap))
                               (expr
                                (when pass-need
                                  `(table-pass ,tab
                                       ,inits

                                       (list ,@(loop
                                                  for id in pass-need
                                                  collecting
                                                    `(symbol-macrolet
                                                         ,(gethash id pass->initsymmap)
                                                       ,(gethash id
                                                                 id->return))))
                                       ,lfield-bindings
                                     ,@(loop
                                          for id in pass-need
                                          collecting
                                          ;; id->initsymmap is a map
                                          ;; from id to the bindings
                                          ;; given to symbol-macrolet
                                            `(symbol-macrolet
                                                 ,(gethash id pass->initsymmap)
                                               ,(sublis
                                                 (loop
                                                    for old being the hash-keys
                                                    in (gethash id pass->lfieldsymmap)
                                                    for new being the hash-values
                                                    in (gethash id pass->lfieldsymmap)
                                                    collect (cons (copy-list
                                                                   `(field ,old))
                                                                  (copy-list
                                                                   `(field ,new))))
                                                 (gethash id id->body)
                                                 :test #'equal))))))
                               (id-key (list tab
                                             `(pass ,pass-number))))
                          ;; create pass target
                          (setf (gethash (aif
                                          (gethash id-key
                                                   *pass-merge-symmap*)
                                          it
                                          ;; set symbol in symmap & return:
                                          (setf (gethash id-key
                                                         *pass-merge-symmap*)
                                                (gsym 'pass-merge)))
                                         result-table)
                                (make-instance 'target
                                               ;;:id id-key
                                               :id  (gethash id-key
                                                             *pass-merge-symmap*)
                                               :expr expr
                                               :deps (list->set
                                                      (mapcan
                                                       #'copy-list
                                                       (loop
                                                          for i in pass
                                                          collecting
                                                            (gethash i id->deps))))
                                               :pdeps (list->set
                                                       (mapcan
                                                        #'copy-list
                                                        (loop
                                                           for i in pass
                                                           collecting
                                                             (gethash i id->pdeps))))
                                               :stat nil
                                               :val nil))
                          ;; Set pass target status based on immediate
                          ;; dependents
                          (setf
                           (target-stat
                            (gethash (gethash id-key
                                              *pass-merge-symmap*)
                                     result-table))
                           (every (lambda (p)
                                    (target-stat
                                     (gethash p target-table)))
                                  pass))

                          ;; create new targets for final results:
                          (let ((tabsym
                                 (gethash id-key
                                          *pass-merge-symmap*)))
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
                                                  :stat old-stat)))))))))))
        ;; Copy old bindings:
        (loop
           for id being the hash-keys in target-table
           when (not (member id table-pass-ids
                             :test #'equal))
           do (setf (gethash id result-table)
                    (gethash id target-table)))
        result-table))))
