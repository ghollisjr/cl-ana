(in-package makeres-tabletrans)

;;;; Idea for algorithm:
;;;;
;;;; * First apply pass-collapse, then pass-merge.
;;;;
;;;; - Extend table-pass to have a per-loop binding section as well;
;;;;   this can serve dual purposes of
;;;;
;;;;   1. table-collapse field variable bindings
;;;;
;;;;   2. logical columns can be added to tables simply using this
;;;;      binding list
;;;;
;;;; * Bindings for all collapsed table fields can be created as
;;;;   bindings for the collapsed table-pass targets (in principle only
;;;;   one target needs the bindings as they will be merged together),
;;;;   and each collapsed table body can be wrapped in a macrolet
;;;;   redefining the field macro to return the appropriate symbol
;;;;   present in the bindings.
;;;;
;;;; * Only if a more fundamental table has no need of passing should
;;;;   there be a second pass over the dependent table; this is the
;;;;   idea of the optimization in the first place: save time spent
;;;;   reading from disc by computing as much as possible per loaded
;;;;   data.

;;;; logical tables need special treatment, they are not simply
;;;; logical results:
;;;;
;;;; A physical table's first pass can (sometimes) be collapsed into a
;;;; pass of the source table, and targets which depend on information
;;;; from the first pass can be acquired via a pass over the saved
;;;; physical table (i.e. the second pass is a pass over the resulting
;;;; table, but the first pass is in a special context inside a pass
;;;; over the source table).
;;;;
;;;; A logical table's passes must always be a pass over the contents
;;;; over the source table, so if the logical table were a target it
;;;; would not be handled correctly.  Therefore logical tables are in
;;;; a real sense nonexistant, just place holders for other
;;;; computations.  References to a logical table in other targets
;;;; must be converted into references to the source table.

;;;; As logical tables handling consists of pass collapsing,
;;;; pass-collapse seems to be the appropriate place to handle them.

;; Operators tab and ltab, special cases of table-pass which result in
;; physical or logical tables.  Assumes that push-field will be
;; defined as a macrolet in the body which pushes fields into the
;; resulting table.  I'd like to make this automatic, but as my
;; example with hdf-tables shows: each table may need convoluted init
;; bindings which check the status of the result table, etc.
(defmacro tab (source opener inits
               &body body)
  "Operator for generating physical tables via table-pass.  Returns a
table-pass form (so you can run macroexpand on it in a graph
transformation).

source is the source table to be iterated over.

opener should be a closure which accepts a single keyword argument.
When given keyword argument :read it should return an open table
object ready for reading, and when given keyword argument :write
should return a table object ready for writing.  opener should handle
all necessary calls to table-close as well as managing e.g. open
files.

inits and lfields are passed directly to table-pass.

body will be placed in a macrolet which macrolets push-field,
accepting all arguments to table-push-field minus the destination
table (will be supplied the result table)."
  (alexandria:with-gensyms (closure result)
    `(table-pass ,source
         (,@inits
          (,closure ,opener)
          (,result (funcall ,closure :write)))
         (funcall ,closure :read)
         ()
       (macrolet ((push-fields (&rest fields)
                    `(table-push-fields ,',result
                       ,@fields)))
         ,@body))))

(defmacro ltab (source inits &body body)
  "Like tab, but for logical tables.  Returns nil.  Requires special
  treatment since logical tables don't yield a result.  Arguments are
  simply for tracking the logical table."
  nil)

;;;; My mind is worn out, but the algorithm seems clearer to me now:
;;;; the model is the limit of slow disc access, so that you should do
;;;; as much computing as possible per disc read.  This means that
;;;; instead of generating an output physical table and then running
;;;; subsequent passes over the physical table, you should condense as
;;;; much work as possible into passes over the most fundamental
;;;; tables which still need passes.  Once the most fundamental tables
;;;; no longer need to be passed over, the reductions due to generated
;;;; tables can be generated from saved tables.
;;;;
;;;; So this transformation will have to know about in which order
;;;; passes should occur and connect
;;;;
;;;; In this framework, logical tables are just physical tables which
;;;; always need to be read from the source table and are never
;;;; actually written to.  They still require special treatment
;;;; however since they should always be condensed to a pass over the
;;;; source table, whereas physical tables can be used once source
;;;; passes are no longer necessary.
;;;;
;;;; pass-collapse should happen before pass-merge simply to reuse the
;;;; pass numbering capability.  Technically pass-collapse doesn't
;;;; need to keep track of specific passes, only make sure that
;;;; logical tables are always collapsed, and physical tables are
;;;; collapsed in a way consistent with the number of passes required
;;;; by source tables.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; more shitty code walking
  (defun find-push-fields (form)
    "Returns the list of all arguments given to all instances of
push-fields in the form which are not within a macrolet definition."
    (when (listp form)
      (cond
        ((eq (first form)
             'push-fields)
         (copy-list (rest form)))
        ((eq (first form)
             'macrolet)
         (mapcan #'find-push-fields (rest (rest form))))
        (t
         (mapcan #'find-push-fields form)))))

  (defun replace-push-fields (form replacement)
    "Replaces push-fields within form with replacement as long as it's
not inside a macrolet definition"
    (labels ((rec (frm)
               (if (listp frm)
                   (cond
                     ((eq (first frm)
                          'push-fields)
                      replacement)
                     ((eq (first frm)
                          'macrolet)
                      `(macrolet ,(second frm)
                         ,@(mapcar #'rec (rest (rest frm)))))
                     (t
                      (mapcar #'rec frm)))
                   frm)))
      (rec form)))

  (defun pass-collapse (target-table)
    ;; Initialize hash table for *proj->tab->ltab-lfields*
    (when (not (gethash (project) *proj->tab->lfields*))
      (setf (gethash (project) *proj->tab->lfields*)
            (make-hash-table :test 'equal)))
    (setf (gethash (project) *proj->tab->ltab-lfields*)
          (make-hash-table :test 'equal))
    ;; the *proj->tab->ltab-lfields* table needs to be updated while
    ;; collapse!-ing; any sorce tables need to have the lfields from a
    ;; logical table appended (without repeats, i.e. reduce with
    ;; adjoin) to the mapped list.
    (let ((result
           ;;(make-hash-table :test 'equal))
           (alexandria:copy-hash-table target-table))
          ;; set of ltab ids
          (ltab-ids nil)
          ;; map from reduction id to logical table id
          (red->ltab (make-hash-table :test 'equal))
          ;; map from ltab id to list of reduction ids:
          (ltab->reds (make-hash-table :test 'equal)))
      ;; find logical reduction targets
      (loop
         for id being the hash-keys in target-table
         for tar being the hash-values in target-table
         do (destructuring-bind (progn &rest body)
                (target-expr tar)
              (when (eq (first (first body))
                        'ltab)
                (setf ltab-ids
                      (adjoin id ltab-ids
                              :test #'equal)))))
      ;; and now for the reductions of ltab objects:
      (loop
         for id being the hash-keys in target-table
         for tar being the hash-values in target-table
         do (destructuring-bind (progn &rest body)
                (target-expr tar)
              (when (or (eq (first (first body))
                            'table-pass)
                        (eq (first (first body))
                            'dotab)
                        (eq (first (first body))
                            'tab)
                        (eq (first (first body))
                            'ltab))
                (destructuring-bind (tab-op src &rest xs)
                    (first body)
                  (when (and (listp src)
                             (eq (first src) 'res)
                             (member (second src)
                                     ltab-ids
                                     :test #'equal))
                    ;; investigate
                    (setf (gethash id red->ltab)
                          (second src))
                    (push id
                          (gethash (second src) ltab->reds)))))))

      ;; Collapse all logical reductions
      ;;
      ;; algorithm: Looks like I'll have to do something similar to
      ;; pass-merge: for each logical table, create a target which
      ;; will execute all of the reduction bodies wherever push-fields
      ;; is located, appropriately modifying the inits & lfields of
      ;; the ltab target.  The return form should call setresfn on all
      ;; the reduction ids, setting the final result.  This means that
      ;; the targets for reductions should have their status set to t
      ;; in (gethash (project) *target-tables*) and (gethash (project)
      ;; *fin-target-tables*) when available.

      ;; start with most fundamental ltabs and collapse by recursion.
      ;; This function should be mapped across most fundamental ltabs.
      (let ((ltabs-copy (copy-list ltab-ids)))
        (labels ((sync-ltab->reds ()
                   (setf ltab->reds
                         (make-hash-table :test 'equal))
                   (loop
                      for id being the hash-keys in target-table
                      for tar being the hash-values in target-table
                      do (destructuring-bind (progn &rest body)
                             (target-expr tar)
                           (when (or (eq (first (first body))
                                         'table-pass)
                                     (eq (first (first body))
                                         'tab)
                                     (eq (first (first body))
                                         'ltab))
                             (destructuring-bind (tab-op src &rest xs)
                                 (first body)
                               (when (and (listp src)
                                          (eq (first src)
                                              'res)
                                          (member (second src) ltabs-copy
                                                  :test #'equal))
                                 (push id
                                       (gethash (second src) ltab->reds))))))))
                 (sml-bindings (inits)
                   ;; returns list of symbol-macrolet bindings for inits
                   (loop
                      for i in inits
                      collecting (list i (gensym))))
                 (init-bindings (ltab-inits inits sml-bindings)
                   ;; returns new inits given ltab-inits, inits and sml-bindings
                   (list->set
                    (append
                     ltab-inits
                     (mapcar (lambda (init sml)
                               (append (second sml)
                                       (rest init)))
                             inits sml-bindings))
                    #'equal))
                 (collapse-expr (expr ltab)
                   ;; returns collapsed expression through ltab
                   (destructuring-bind (progn
                                         (ltab-op ltab-source ltab-inits
                                                  &rest ltab-body))
                       (target-expr (gethash ltab result))
                     (destructuring-bind (progn
                                           (tab-op &rest args))
                         expr
                       (cond
                         ((eq tab-op 'ltab)
                          (destructuring-bind (source inits &rest body)
                              args
                            (let* ((sml-bindings (sml-bindings inits))
                                   (init-bindings
                                    (init-bindings ltab-inits
                                                   inits
                                                   sml-bindings)))
                              (copy-list
                               `(progn
                                  (ltab ,ltab-source
                                      ,init-bindings
                                    (symbol-macrolet ,sml-bindings
                                      ,(replace-push-fields
                                        `(progn ,@ltab-body)
                                        `(progn ,@body)))))))))
                         ((eq tab-op 'tab)
                          (destructuring-bind (source opener inits &rest body)
                              args
                            (let* ((sml-bindings (sml-bindings inits))
                                   (init-bindings
                                    (init-bindings ltab-inits
                                                   inits
                                                   sml-bindings)))
                              (copy-list
                               `(progn
                                  (tab ,ltab-source
                                      ,opener
                                      ,init-bindings
                                    (symbol-macrolet ,sml-bindings
                                      ,(replace-push-fields
                                        `(progn ,@ltab-body)
                                        `(progn ,@body)))))))))
                         ((eq tab-op 'dotab)
                          (destructuring-bind (source inits return &rest body)
                              args
                            (let* ((sml-bindings (sml-bindings inits))
                                   (init-bindings
                                    (init-bindings ltab-inits
                                                   inits
                                                   sml-bindings)))
                              (copy-list
                               `(progn
                                  (dotab ,ltab-source
                                      ,init-bindings
                                      ,return
                                    (symbol-macrolet ,sml-bindings
                                      ,(replace-push-fields
                                        `(progn ,@ltab-body)
                                        `(progn ,@body)))))))))
                         ((eq tab-op 'table-pass)
                          (destructuring-bind (source inits return lfields &rest body)
                              args
                            (let* ((sml-bindings (sml-bindings inits))
                                   (init-bindings
                                    (init-bindings ltab-inits
                                                   inits
                                                   sml-bindings)))
                              (copy-list
                               `(progn
                                  (table-pass ,ltab-source
                                      ,init-bindings
                                      ,return
                                      ,lfields
                                    (symbol-macrolet ,sml-bindings
                                      ,(replace-push-fields
                                        `(progn ,@ltab-body)
                                        `(progn ,@body)))))))))))))
                 (collapse! (ltab)
                   ;; collapses logical table ltab
                   ;;
                   ;; collapse all ltab reductions below the
                   ;; current reduction.  (This also eliminates
                   ;; the entire chain of ltabs chained
                   ;; together with this one via recursion)
                   (setf ltabs-copy
                         (remove ltab ltabs-copy))
                   (destructuring-bind (progn-op
                                        (ltab-op source inits &rest body))
                       (target-expr (gethash ltab result))
                     ;; add lfields to *proj->tab->ltab-lfields*:
                     ;; (when (not (gethash (project)
                     ;;                     *proj->tab->ltab-lfields*))
                     ;;   (setf (gethash (project)
                     ;;                  *proj->tab->ltab-lfields*)
                     ;;         (make-hash-table :test 'equal)))
                     ;; (when (not (gethash (project)
                     ;;                     *proj->tab->lfields*))
                     ;;   (setf (gethash (project)
                     ;;                  *proj->tab->lfields*)
                     ;;         (make-hash-table :test 'equal)))

                     ;; collapse dependent ltabs:
                     (loop for red in (gethash ltab ltab->reds)
                        do (let* ((rtar (gethash red result))
                                  (expr (target-expr rtar)))
                             (destructuring-bind
                                   (progn-op
                                    (tab-op &rest args))
                                 expr
                               (when (eq tab-op 'ltab)
                                 ;; recurse
                                 (collapse! red)))))

                     ;; order should be:
                     ;; 
                     ;; 1. collapse ltab reds
                     ;; 
                     ;; 2. copy ltab-lfields for this ltab into
                     ;;    lfields for this ltab
                     ;;
                     ;; 3. copy find-push-fields... into ltab-lfields
                     ;;    for this ltab
                     ;;
                     ;; 4. copy lfields for this ltab into
                     ;;    ltab-lfields for source

                     (let* ((tab->ltab-lfields
                             (gethash (project)
                                      *proj->tab->ltab-lfields*))
                            (tab->lfields
                             (gethash (project)
                                      *proj->tab->lfields*)))
                       (setf (gethash ltab tab->lfields)
                             (union (gethash ltab tab->lfields)
                                    (union
                                     (gethash ltab tab->ltab-lfields)
                                     (find-push-fields body)
                                     :test #'eq
                                     :key #'first)
                                    :test #'eq
                                    :key #'first))
                       (when (and (listp source)
                                  (eq (first source)
                                      'res))
                         (let ((src (second source)))
                           (setf (gethash src tab->ltab-lfields)
                                 (union (gethash src tab->ltab-lfields)
                                        (gethash ltab tab->lfields)
                                        :test #'eq
                                        :key #'first)))))

                     ;; for each reduction directly under the
                     ;; current reduction (red), collapse it with
                     ;; ltab

                     (loop
                        for r in (gethash ltab ltab->reds)
                        do
                          (let* ((rtar (gethash r result))
                                 (expr (target-expr rtar)))
                            (destructuring-bind
                                  (progn-op
                                   (tab-op &rest args))
                                expr
                              (setf (gethash r result)
                                    (make-target
                                     r
                                     (collapse-expr expr
                                                    ltab)
                                     :val (target-val rtar)
                                     :stat (target-stat rtar))))))
                     ;; re-sync ltab->reds
                     (sync-ltab->reds))))
          ;; apply collapse!
          (when ltabs-copy
            (do ()
                ((null ltabs-copy) nil)
              (let* ((ltab (pop ltabs-copy)))
                (collapse! ltab))))))

      ;; Collapse physical table passes when possible
      ;;
      ;; bare minimum (no collapsing):
      (let ((tab-ids nil))
        ;; get tab-ids
        (loop
           for id being the hash-keys in result
           for tar being the hash-values in result
           do (destructuring-bind (progn
                                    &rest body)
                  (target-expr tar)
                (when (and body
                           (listp (first body))
                           (eq (first (first body))
                               'tab))
                  (setf tab-ids
                        (adjoin id tab-ids
                                :test #'equal)))))
        ;; macroexpand-1 the tab macros:
        (loop
           for tab in tab-ids
           do (let* ((tar (gethash tab result))
                     (expr (target-expr tar)))
                (destructuring-bind (progn
                                      tab-form)
                    expr
                  (setf (target-expr tar)
                        (list 'progn
                              (macroexpand-1 tab-form)))))))
      
      ;; return transformed graph
      result)))
