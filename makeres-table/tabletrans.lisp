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

(in-package :cl-ana.makeres-table)

(defvar *print-progress* nil
  "Set this to nil if you don't want to see progress messages printed;
set this to an integer value to specify the number of rows at which to
print a progress update message.  Note that this should only be used
for tables which know their size (so CSV tables don't work with
this).")

(defun table-reduction? (expr)
  "True if expr is a dotab, ltab or tab expression"
  (when (and expr
             (listp expr))
    (destructuring-bind (progn &rest forms) expr
      (when (listp (first forms))
        (let ((tab-op (first (first forms))))
          (member tab-op (list 'table-pass 'dotab 'ltab 'tab)
                  :test 'eq))))))

(defun table-target? (id)
  (let ((tar (gethash id (target-table))))
    (and (target-stat tar)
         (or (tab? (target-expr tar))
             (ltab? (target-expr tar))
             (typep (target-val tar) 'table)
             (typep (target-val tar) 'reusable-table)))))

(defun table-pass? (expr)
  "True if expr is a table-pass expression"
  (when expr
    (destructuring-bind (progn &rest forms) expr
      (let ((tab-op (first (first forms))))
        (eq tab-op 'table-pass)))))

(defun srctab? (expr)
  "True if expr is a srctab expression"
  (when (and expr
             (listp expr)
             (listp (second expr)))
    (destructuring-bind (progn &rest forms) expr
      (let ((tab-op (first (first forms))))
        (eq tab-op 'srctab)))))
        

(defun dotab? (expr)
  "True if expr is a dotab expression"
  (when expr
    (destructuring-bind (progn &rest forms) expr
      (let ((tab-op (first (first forms))))
        (eq tab-op 'dotab)))))

(defun tab? (expr)
  "True if expr is a tab expression"
  (when (and expr
             (listp expr))
    (destructuring-bind (progn &rest forms) expr
      (when (listp (first forms))
        (let ((tab-op (first (first forms))))
          (eq tab-op 'tab))))))

(defun ltab? (expr)
  "True if expr is an ltab expression"
  (when (and expr
             (listp expr))
    (destructuring-bind (progn &rest forms) expr
      (when (listp (first forms))
        (let ((tab-op (first (first forms))))
          (eq tab-op 'ltab))))))

(defun resform? (expr)
  "Returns true if expr is of the form (res x)"
  (and (listp expr)
       (eq (first expr)
           'res)))

(defun unres (expr)
  "Gets id from a res form if it is a res form, if not, returns expr."
  (if (resform? expr)
      (second expr)
      expr))

(defun mkres (expr)
  "Ensures that expr is a res form"
  (if (resform? expr)
      expr
      (list 'res expr)))

(defun table-reduction-source (expr)
  "Returns source for table reduction, nil if expr is not of a
table-reduction."
  (when (table-reduction? expr)
    (cadadr expr)))

(defun (setf table-reduction-source) (value expr)
  (when (table-reduction? expr)
    (setf (cadadr expr)
          value)))

(defun table-reduction-inits (expr)
  "Returns init bindings when expr is a table-reduction, nil
otherwise."
  (when (table-reduction? expr)
    (destructuring-bind (progn tab-form) expr
      (elt tab-form
           2))))

;; call on table-pass or dotab only
(defun table-reduction-return (expr)
  (when (or (dotab? expr)
            (table-pass? expr))
    (destructuring-bind (progn tab-form) expr
      (elt tab-form 3))))

(defun table-reduction-body (expr)
  (when (table-reduction? expr)
    (destructuring-bind (progn tab-form) expr
      (cond
        ((table-pass? expr)
         (nthcdr 5 tab-form))
        ((dotab? expr)
         (nthcdr 4 tab-form))
        ((tab? expr)
         (nthcdr 4 tab-form))
        ((ltab? expr)
         (nthcdr 3 tab-form))))))

(defun immediate-reductions (target-table tab)
  "Returns list of immediately dependent table reductions for a
table"
  (remove-if-not (lambda (id)
                   (let* ((tar (gethash id target-table))
                          (expr (target-expr tar)))
                     (and (table-reduction? expr)
                          (equal (unres (table-reduction-source expr))
                                 tab))))
                 (hash-keys target-table)))

(defun ltab-chains (target-table src)
  "Returns all ltab chains stemming from src"
  (labels ((rec (red &optional chain)
             (if (ltab? (target-expr (gethash red target-table)))
                 (let* ((imms (immediate-reductions target-table red)))
                   (mapcan (lambda (r)
                             (copy-list (rec r (cons red chain))))
                           imms))
                 (list (reverse (cons red chain))))))
    (mapcan (lambda (r)
              (rec r (list src)))
            (immediate-reductions target-table src))))

(defun ltab-chained-reductions (target-table src)
  "Returns all reductions directly from ltab chains stemming from src"
  (mapcar #'alexandria:last-elt
          (ltab-chains target-table src)))

(defun necessary-pass-reductions (target-table tab)
  "Returns list of reductions of a table which must be computed via a
pass over the table; equivalent to the union set of all immediate
non-ltab reductions and any reductions chained directly to tab via
logical tables."
  (list->set
   (append (remove-if (lambda (id)
                        (ltab?
                         (target-expr
                          (gethash id target-table))))
                      (immediate-reductions target-table tab))
           (ltab-chained-reductions target-table tab))
   #'equal))

(defun chained-reductions (target-table src)
  "Returns list of ids for targets from target-table which are
connected via a chain of reductions from src."
  (let ((imm-reds (immediate-reductions target-table src)))
    (when imm-reds
      (append imm-reds
              (mapcan (lambda (red)
                        (chained-reductions target-table red))
                      (remove-if-not
                       (lambda (red)
                         (table-reduction?
                          (target-expr (gethash red target-table))))
                       imm-reds))))))

(defun group-ids-by-pass (target-table src &optional dep<)
  "Groups all ids from target-table according the the pass required
over src using the dependency checker dep<."
  (let* ((dep< (if dep<
                   dep<
                   (dep< target-table)))
         (chained
          (chained-reductions target-table src))
         (sorted-ids
          (remove-if-not (lambda (x)
                           (member x chained :test #'equal))
                         (depsort-graph target-table dep<))))
    (when sorted-ids
      (let ((pass (list (pop sorted-ids)))
            (result nil))
        (labels ((rec ()
                   (dolist (i sorted-ids)
                     (when (every (lambda (p)
                                    (funcall dep<
                                             i
                                             p))
                                  pass)
                       (push i pass)))
                   (push (reverse pass) result)
                   (setf sorted-ids
                         (remove-if (lambda (sid)
                                      (member sid pass
                                              :test #'equal))
                                    sorted-ids))
                   (if sorted-ids
                       (progn
                         (setf pass (list (pop sorted-ids)))
                         (rec))
                       (nreverse result))))
          (rec))))))

(defun ultimate-source-tables (target-table &optional ignore)
  "Returns list of source table ids which are not table reductions of
non-ignored sources."
  (let ((srcs nil)
        (reds nil))
    (loop
       for id being the hash-keys in target-table
       for tar being the hash-values in target-table
       do (let ((expr (target-expr tar)))
            (when (table-reduction? expr)
              (let* ((raw-src (table-reduction-source expr))
                     (src (if (and (listp raw-src)
                                   (eq (first raw-src) 'res))
                              (second raw-src)
                              raw-src)))
                (when (not (member src
                                   ignore
                                   :test #'equal))
                  (push id reds)
                  (setf srcs
                        (adjoin src
                                srcs
                                :test #'equal)))))))
    (set-difference srcs reds)))

(defun removed-source-dep< (target-table)
  (let ((depmap (make-hash-table :test 'equal)))
    (labels ((rec (id)
               ;; returns full list of dependencies for id, ignoring
               ;; t-stat targets (otherwise we neglect possible
               ;; optimizations)
               (let ((deps
                      (remove-if
                       (lambda (d)
                         (target-stat (gethash d target-table)))
                       (copy-list (target-deps (gethash id target-table))))))
                 (when deps
                   (reduce (lambda (ds d)
                             (adjoin d ds :test #'equal))
                           (mapcan #'rec deps)
                           :initial-value
                           (let ((expr
                                  (target-expr (gethash id target-table))))
                             (if (table-reduction? expr)
                                 (destructuring-bind (progn tab-form) expr
                                   (cl-ana.makeres::find-dependencies (cddr tab-form)
                                                                      'res))
                                 deps)))))))
      (loop
         for id being the hash-keys in target-table
         do (setf (gethash id depmap)
                  (rec id)))
      (lambda (x y)
        (not (member y (gethash x depmap)
                     :test #'equal))))))

(defun removed-ltab-source-dep< (target-table)
  (let ((depmap (make-hash-table :test 'equal)))
    (labels ((rec (id)
               ;; returns full list of dependencies for id, ignoring
               ;; t-stat dependencies (otherwise we neglect possible
               ;; optimizations).
               (let ((deps
                      (remove-if
                       (lambda (d)
                         (target-stat (gethash d target-table)))
                       (copy-list (target-deps (gethash id target-table))))))
                 (when deps
                   (reduce (lambda (ds d)
                             (adjoin d ds :test #'equal))
                           (mapcan #'rec deps)
                           :initial-value
                           (let ((expr
                                  (target-expr (gethash id target-table))))
                             (if (ltab? expr)
                                 (destructuring-bind (progn tab-form) expr
                                   (cl-ana.makeres::find-dependencies (cddr tab-form)
                                                                      'res))
                                 deps)))))))
      (loop
         for id being the hash-keys in target-table
         do (setf (gethash id depmap)
                  (rec id)))
      (lambda (x y)
        (not (member y (gethash x depmap)
                     :test #'equal))))))

;;; Table pass expression components

;; Context tree functions:
(defun node (id content &rest children)
  (apply #'list id content children))

(defun node-id (node)
  (first node))

(defun (setf node-id) (value node)
  (setf (first node) value))

(defun node-content (node)
  (second node))

(defun (setf node-content) (value node)
  (setf (second node) value))

(defun node-children (node)
  (cddr node))

(defun (setf node-children) (value node)
  (setf (cddr node) value))

(defun tree-ids (node)
  "Returns list of ids stored in node"
  (let ((result nil))
    (labels ((rec (n)
               (let ((id (node-id n)))
                 (setf result
                       (adjoin id result :test #'equal))
                 (mapcar #'rec (node-children n)))))
      (rec node))
    result))

(defun node-subcontent (node)
  "Returns set of contents contained by node and all its children"
  (list->set
   (append (node-content node)
           (mapcan
            (lambda (x)
              (copy-list (node-subcontent x)))
            (node-children node)))
   #'equal))

;; must be given complete target graph, not just the null-stat targets
(defun table-reduction-context-tree (graph src ids)
  "Returns tree of contexts each pass would be inside if collapsed up
to src.  Physical table reductions are treated as reductions with
themselves as context."
  (let (;; map from source to immediate reductions used by ids
        (source->reds (make-hash-table :test 'equal)))
    (labels ((build-source->reds (id)
               ;; builds map from source table to immediate reductions
               (when (and (gethash id graph)
                          (not (equal id src)))
                 (let ((source (unres
                                (table-reduction-source
                                 (target-expr
                                  (gethash id graph))))))
                   (setf (gethash source source->reds)
                         (adjoin id (gethash source source->reds)
                                 :test #'equal))
                   (when (not (equal source src))
                     (build-source->reds source)))))
             (source->tree (source)
               ;; generates context tree for ids from source
               ;;
               ;; context tree nodes consist of source, content, and
               ;; child nodes.
               ;;
               ;; content is list of immediate reduction ids
               (let* (;; all reductions encountered
                      (reds (gethash source source->reds))
                      ;; reductions which need to be placed in contexts
                      (need-context-reds
                       (append (remove-if
                                (lambda (red)
                                  (or (target-stat (gethash red graph))
                                      (ltab? (target-expr (gethash red graph)))))
                                reds)
                               (when (and (gethash source graph)
                                          (tab? (target-expr (gethash source graph)))
                                          (member source ids :test #'equal))
                                 (list source))))
                      ;; reductions used as sources
                      (source-reds
                       (remove-if-not (lambda (red)
                                        (gethash red source->reds))
                                      reds))
                      ;; child nodes:
                      (children (mapcar #'source->tree source-reds)))
                 (apply #'node
                        source
                        ;; remove any reds needing context which are
                        ;; covered by some child node:
                        (let ((subcontent
                               (list->set
                                (mapcan (lambda (x)
                                          (copy-list (node-subcontent x)))
                                        children)
                                #'equal)))
                          (remove-if (lambda (r)
                                       (member r subcontent
                                               :test #'equal))
                                     need-context-reds))
                        children)))
             (tab-cleanup (tree)
               ;; Ensures that the tree obtained from source->tree
               ;; properly stores physical table nodes.  Physical
               ;; table nodes should never be content of the immediate
               ;; source table, they should be child nodes which
               ;; contain at least themselves as content.
               (let* ((source (node-id tree))
                      (content (node-content tree))
                      (newcontent (remove-if (lambda (id)
                                               (and (tab?
                                                     (target-expr (gethash id graph)))
                                                    (not (equal id source))))
                                             content))
                      (tabs (remove-if-not (lambda (id)
                                             (and (tab?
                                                   (target-expr (gethash id graph)))
                                                  (not (equal id source))))
                                           content))
                      (children (copy-tree (node-children tree))))
                 ;; modify children appropriately
                 (loop
                    for tab in tabs
                    do (loop
                          for child in children
                          when (and (equal (node-id child)
                                           tab)
                                    (not (member tab (node-content child))))
                          do (progn
                               (push tab (node-content child))
                               (return))
                          finally (push (node tab (list tab))
                                        children)))
                 ;; return result
                 (apply #'node
                        source
                        newcontent
                        (mapcar #'tab-cleanup
                                children)))))
      (mapcar #'build-source->reds ids)
      (tab-cleanup
       (source->tree src)))))

;; some shitty code walking
(defun find-push-fields (form)
  "Returns the list of all arguments given to all instances of
push-fields in the form which are not within a macrolet definition."
  (when (and form
             (listp form))
    (cond
      ((eq (first form)
           'push-fields)
       (copy-list (rest form)))
      ((eq (first form)
           'macrolet)
       (mapcan #'find-push-fields (rest (rest form))))
      (t
       (append (find-push-fields (car form))
               (find-push-fields (cdr form)))))))

(defun replace-push-fields (form replacement)
  "Replaces push-fields within form with replacement as long as it's
not inside a macrolet definition"
  (labels ((rec (frm)
             (if (and frm
                      (listp frm))
                 (cond
                   ((eq (first frm)
                        'push-fields)
                    replacement)
                   ((eq (first frm)
                        'macrolet)
                    `(macrolet ,(second frm)
                       ,@(mapcar #'rec (rest (rest frm)))))
                   (t
                    (cons (rec (first frm))
                          (rec (rest frm)))))
                 frm)))
    (rec form)))

;; must be given complete target graph, not just the null-stat targets
(defun make-pass-target-expr (graph src pass)
  "Return expression for pass target over src, collapsing all results
from pass up to src."
  (flet ((htref (ht &rest keys)
           ;; looks up values stored in nested hash tables, one key
           ;; per hash table layer
           (reduce (lambda (h k)
                     (gethash k h))
                   keys
                   :initial-value ht)))
    ;; Context handling:
    (let* (;; tree of contexts, each node contains context name,
           ;; reduction ids needing to be placed in this context, and
           ;; sub context trees.
           (context-tree
            (table-reduction-context-tree graph src pass))
           ;; set of reductions generated:
           (reductions
            (remove src
                    (list->set
                     (labels ((rec (n)
                                (cons (node-id n)
                                      (append
                                       (node-content n)
                                       (mapcan #'rec (node-children n))))))
                       (rec context-tree)))))
           ;; map from reduction id to map from init binding variable
           ;; to gsym
           (reduction->initsym->gsym
            (make-hash-table :test 'equal))
           ;; map from reduction id to map from init binding variable to
           ;; form.
           (reduction->initsym->expr
            (make-hash-table :test 'equal))
           ;; map from reduction to return form:
           (reduction->return
            (make-hash-table :test 'equal))
           ;; map from tab reduction to expanded form (needed due to
           ;; with-gensyms in the body)
           (tab-expanded-expr (make-hash-table :test 'equal)))
      ;; Initialize context maps:
      (macrolet ((setht (place k)
                   `(setf (gethash ,k ,place)
                          (make-hash-table :test 'equal))))
        (loop
           for r in reductions
           do (progn
                (setht reduction->initsym->gsym r)
                (setht reduction->initsym->expr r)
                (setht reduction->return r))))
      ;; Make maps from initsyms to gsyms and expressions for each
      ;; reduction (reduction->initsym->gsym,
      ;; reduction->initsym->expr) as well as map from reduction to
      ;; return form
      (loop
         for r in reductions
         do (let ((initsym->gsym
                   (gethash r reduction->initsym->gsym))
                  (initsym->expr
                   (gethash r reduction->initsym->expr))
                  (tar (gethash r graph))
                  (processed-initsym-bindings nil))
              ;; returns:
              (setf (gethash r reduction->return)
                    (let ((expr (target-expr tar)))
                      (when (not (ltab? expr))
                        (let ((res
                               (table-reduction-return
                                (if (tab? expr)
                                    (setf (gethash r tab-expanded-expr)
                                          `(progn
                                             ,(macroexpand-1 (second expr))))
                                    expr))))
                          res))))
              ;; inits:
              (loop
                 for (initsym . initexpr)
                 in (table-reduction-inits
                     (let ((expr (target-expr tar)))
                       (if (tab? expr)
                           (gethash r tab-expanded-expr)
                           expr)))
                 do (progn
                      (setf (gethash initsym initsym->gsym)
                            (gsym 'tabletrans))
                      (setf (gethash initsym initsym->expr)
                            (copy-list
                             ;; symbol-macrolet to use gsym bindings
                             `(symbol-macrolet
                                  ,(loop
                                      for s in processed-initsym-bindings
                                      collect `(,s ,(gethash s initsym->gsym)))
                                ,@initexpr)))
                      (push initsym processed-initsym-bindings)))))
      ;; Make body via recursing through context tree

      ;; * Make sure to make use of gsymed inits via symbol-macrolets in
      ;;   pass bodies
      (let* (;; gsymed init bindings for all reductions in pass:
             (inits
              (loop
                 for r in reductions
                 appending
                   (let ((initsym->gsym (gethash r reduction->initsym->gsym))
                         (initsym->expr (gethash r reduction->initsym->expr)))
                     (loop
                        for s being the hash-keys in initsym->gsym
                        for gsym being the hash-values in initsym->gsym
                        collect (list gsym (gethash s initsym->expr))))))
             ;; list of result forms making use of any gsymed values:
             (result-list
              (progn
                `(list
                  ,@(loop
                       for r in pass
                       collect
                         (let ((initsym->gsym (gethash r reduction->initsym->gsym)))
                           `(symbol-macrolet
                                ,(loop
                                    for s being the hash-keys in initsym->gsym
                                    for gsym being the hash-values in initsym->gsym
                                    collect (list s gsym))
                              ,(gethash r reduction->return)))))))
             ;; map from table to lfields for table:
             (tab->lfields
              (gethash (project) *proj->tab->lfields*))
             ;; lfields expanded:
             (lfields
              ;; lfields from source
              (when tab->lfields
                (mapcar (lambda (binding)
                          (cons (first binding)
                                (mapcar #'expand-res-macros
                                        (rest binding))))
                        (gethash src tab->lfields))))
             ;; resulting pass body:
             (body
              (labels
                  ((rec (node)
                     (let* ((c (node-id node))
                            (expr (target-expr (gethash c graph)))
                            (push-field-bindings
                             (cond
                               ((tab? expr)
                                (find-push-fields (gethash c tab-expanded-expr)))
                               ((ltab? expr)
                                (find-push-fields (table-reduction-body expr)))))
                            (push-field-syms
                             (cars push-field-bindings))
                            (push-field-gsyms
                             (loop
                                for b in push-field-syms
                                collecting (gsym 'tabletrans)))
                            (push-field->gsym
                             (let ((ht (make-hash-table :test 'eq)))
                               (loop
                                  for sym in push-field-syms
                                  for gsym in push-field-gsyms
                                  do (setf (gethash sym ht)
                                           gsym))
                               ht))
                            (lfields
                             (let ((tab->lfields
                                    (gethash (project) *proj->tab->lfields*)))
                               (when tab->lfields
                                 (gethash c tab->lfields))))
                            (lfield-syms
                             (cars lfields))
                            (lfield-gsyms
                             (loop
                                for l in lfield-syms
                                collecting (gsym 'tabletrans)))
                            (lfield->gsym
                             (let ((ht (make-hash-table :test 'eq)))
                               (loop
                                  for lfield in lfield-syms
                                  for gsym in lfield-gsyms
                                  do (setf (gethash lfield ht)
                                           gsym))
                               ht))
                            (lfield-bindings
                             (when lfields
                               (mapcar
                                (lambda (binding)
                                  (cons
                                   (first binding)
                                   (sublis
                                    (append
                                     (loop
                                        for lfield in lfield-syms
                                        when (not (eq (first binding)
                                                      lfield))
                                        collecting
                                          (cons `(field ,lfield)
                                                (gethash lfield lfield->gsym)))
                                     (loop
                                        for push-field in push-field-syms
                                        collecting
                                          (cons `(field ,push-field)
                                                (gethash push-field
                                                         push-field->gsym))))
                                    (mapcar #'expand-res-macros
                                            (rest binding))
                                    :test #'equal)))
                                lfields)))
                            (olet-field-bindings
                             (append push-field-bindings lfield-bindings))
                            (olet-field-gsyms
                             (append
                              push-field-gsyms
                              lfield-gsyms))
                            (children-exprs
                             (when (node-children node)
                               (mapcar #'rec
                                       (node-children node))))
                            (sub-body
                             ;; create push-fields and lfields bindings:
                             `(olet ,(loop
                                        for (field form) in olet-field-bindings
                                        for gsym in olet-field-gsyms
                                        collect `(,gsym ,form))
                                ;; replace (field x) with x for x for every
                                ;; x in the push-field-bindings
                                ,@(sublis
                                   (loop
                                      for gsym in olet-field-gsyms
                                      for (field form) in olet-field-bindings
                                      collect (cons `(field ,field) gsym))
                                   (append
                                    (mapcar
                                     (lambda (id)
                                       `(symbol-macrolet
                                            ,(let ((initsym->gsym
                                                    (gethash
                                                     id
                                                     reduction->initsym->gsym)))
                                                  (when initsym->gsym
                                                    (loop
                                                       for s being the hash-keys
                                                       in initsym->gsym
                                                       for gsym being the hash-values
                                                       in initsym->gsym
                                                       collecting (list s gsym))))
                                          ,@(let
                                             ((expr (target-expr
                                                     (gethash id graph))))
                                             (if
                                              (tab? expr)
                                              `((push-fields
                                                 ,@(find-push-fields
                                                    (table-reduction-body
                                                     (gethash id tab-expanded-expr)))))
                                              (table-reduction-body expr)))))
                                     (progn
                                       (node-content node)))
                                    children-exprs)
                                   :test #'equal))))
                       (if (and (not (equal c src))
                                (table-reduction? expr))
                           (let ((result
                                  (replace-push-fields
                                   `(progn
                                      ,@(table-reduction-body
                                         (if (tab? expr)
                                             (gethash c tab-expanded-expr)
                                             expr)))
                                   sub-body)))
                             result)
                           sub-body))))
                (rec context-tree)))
             (row-var (gsym 'table-pass))
             (nrows-var (gsym 'table-pass))
             (print-pass-targets
              (when *print-progress*
                `((let ((*print-pretty* nil))
                    (format t "Pass over ~a to compute:~%" ',src)
                    ,@(loop
                         for r in pass
                         collecting `(format t "~a~%" ',r))))))
             (print-pass-targets-var
              (gsym 'table-pass))
             (print-progress-inits
              (when *print-progress*
                `((,row-var 0)
                  (,nrows-var (table-nrows ,(if (and (listp src)
                                                     (eq (first src) 'res))
                                                src
                                                `(res ,src))))
                  ;; message specifying what the pass will accomplish
                  (,print-pass-targets-var
                   ,@print-pass-targets))))
             (print-progress
              (when *print-progress*
                `((progn
                    (when (zerop (the fixnum
                                      (mod ,row-var
                                           (the fixnum ,*print-progress*))))
                      (format t "Event ~a, ~$% complete~%"
                              ,row-var
                              (* 1f2
                                 (/ (float ,row-var)
                                    (float ,nrows-var)))))
                    (incf ,row-var))))))
        `(progn
           (table-pass ,(if (and (listp src)
                                 (eq (first src) 'res))
                            src
                            `(res ,src))
               (,@inits
                ,@print-progress-inits)
               ,result-list
               ,lfields
             ,@print-progress
             ,body))))))

(defun set-pass-result-targets! (result-graph id pass)
  "Sets result-graph targets from pass so that they make use of the
  returned results for the pass target id."
  (loop
     for p in pass
     for i from 0
     do (setf (gethash p result-graph)
              (make-target p `(elt (res ,id)
                                   ,i)
                           :val (target-val (gethash p result-graph))
                           :stat (target-stat (gethash p result-graph)))))
  nil)

(defun ht-filter (fn ht)
  "Returns a new hash table with entries from ht only when fn returns
true when given the key and value from ht."
  (let ((result (make-hash-table :test (hash-table-test ht))))
    (loop
       for k being the hash-keys in ht
       for v being the hash-values in ht
       when (funcall fn k v)
       do (setf (gethash k result)
                v))
    result))

(defparameter *table-binding-ops*
  (list 'tab
        'ltab
        'dotab
        'push-fields))

(defun ensure-table-binding-ops ()
  (ensure-binding-ops)
  (symbol-macrolet ((binding-ops
                     (gethash (project) *proj->binding-ops*)))
    (setf binding-ops
          (list->set (append binding-ops
                             *table-binding-ops*)))))

(defun ensure-table-op-expanders ()
  (symbol-macrolet ((op->expander
                     (gethash (project)
                              *proj->op->expander*)))
    ;; Create table & set expanders for cl:
    (ensure-op-expanders)
    ;; Set table expanders:
    ;; ltab
    (setf (gethash 'ltab op->expander)
          (lambda (expander form)
            (destructuring-bind (op source inits &rest body)
                form
              (list* op
                     (funcall expander source)
                     (mapcar (lambda (lst)
                               (destructuring-bind (var binding)
                                   lst
                                 (list var
                                       (funcall expander binding))))
                             inits)
                     (mapcar expander body)))))
    ;; tab
    (setf (gethash 'tab op->expander)
          (lambda (expander form)
            (destructuring-bind (op source inits opener &rest body)
                form
              (list* op
                     (funcall expander source)
                     (mapcar (lambda (lst)
                               (destructuring-bind (var binding)
                                   lst
                                 (list var
                                       (funcall expander binding))))
                             inits)
                     (funcall expander opener)
                     (mapcar expander body)))))
    ;; dotab
    (setf (gethash 'dotab op->expander)
          (lambda (expander form)
            (destructuring-bind (op source inits return &rest body)
                form
              (list* op
                     (funcall expander source)
                     (mapcar (lambda (lst)
                               (destructuring-bind (var binding)
                                   lst
                                 (list var
                                       (funcall expander binding))))
                             inits)
                     (funcall expander return)
                     (mapcar expander body)))))
    ;; push-fields
    (setf (gethash 'push-fields op->expander)
          (lambda (expander form)
            (destructuring-bind (push-fields &rest fields) form
              (list* push-fields
                     (loop
                        for f in fields
                        collect (if (listp f)
                                    (list (first f)
                                          (funcall expander (second f)))
                                    f))))))))

(defun tabletrans (target-table)
  "Performs necessary graph transformations for table operators"
  ;; Close any open tables needing recomputation:
  (loop
     for id being the hash-keys in target-table
     for tar being the hash-values in target-table
     do (let ((val (target-val tar))
              (stat (target-stat tar)))
          (when (and
                 (not stat)
                 (or (typep val 'table)
                     (typep val 'reusable-table))
                 (table-open-p val))
            (table-close val))))

  ;; initialize operator expansion
  (ensure-table-binding-ops)
  (ensure-table-op-expanders)
  ;; clear gsyms
  (clrgsym 'tabletrans)
  ;; establish *proj->tab->lfields*:
  (when (not (gethash (project) *proj->tab->lfields*))
    (setf (gethash (project) *proj->tab->lfields*)
          (make-hash-table :test 'equal)))
  (let* ((graph (copy-target-table target-table))
         ;; special dep< for treating reductions as if they did not
         ;; depend on src as a source table, but preserving other
         ;; dependencies as a consequence of being a reduction of the
         ;; source.
         (remsrc-dep<
          (removed-source-dep< target-table))
         ;; special dep< which only adds ltabs sources as dependencies
         ;; when used somewhere other than as the source additionally.
         (remltab-dep< (removed-ltab-source-dep< target-table))
         ;; result
         (result-graph
          (copy-target-table target-table))
         ;; list of source targets already processed
         (processed-srcs nil)
         ;; list of reduction targets already processed:
         (processed-reds nil))
    (labels
        ((trans ()
           (let ((srcs
                  ;; not sure if subtracting processed-srcs is really
                  ;; necessary, but I'm leaving it in until further
                  ;; testing confirms it's unnecessary
                  (set-difference
                   (ultimate-source-tables graph processed-srcs)
                   processed-srcs)))
             (when srcs
               (dolist (src srcs)
                 (push src processed-srcs)
                 (let ((ltabs
                        (list->set
                         (alexandria:flatten
                          (mapcar #'butlast
                                  (mapcar #'rest
                                          (ltab-chains (target-table)
                                                       src)))))))
                   (setf processed-srcs
                         (list->set
                          (append processed-srcs
                                  ltabs))))
                 (let* (;; reductions which must be computed via a
                        ;; pass over src
                        (nec-reds
                         (remove-if
                          (lambda (k)
                            (target-stat (gethash k graph)))
                          (remove-if
                           (lambda (k)
                             (member k processed-reds
                                     :test #'equal))
                           (necessary-pass-reductions
                            graph src))))
                        ;; necessary passes:
                        (nec-passes
                         (remove
                          nil
                          (mapcar
                           (lambda (pass)
                             (remove-if (lambda (p)
                                          (ltab? (target-expr (gethash p graph))))
                                        pass))
                           (mapcar
                            (lambda (pass)
                              (remove-if-not (lambda (p)
                                               (member p nec-reds
                                                       :test #'equal))
                                             pass))
                            (group-ids-by-pass
                             graph src remltab-dep<)))))
                        ;; passes relative to ultimate source:
                        (ult-passes
                         (remove
                          nil
                          (mapcar
                           (lambda (pass)
                             (remove-if (lambda (p)
                                          (target-stat (gethash p graph)))
                                        pass))
                           (mapcar
                            (lambda (pass)
                              (remove-if (lambda (p)
                                           (ltab? (target-expr
                                                   (gethash p graph))))
                                         pass))
                            (mapcar
                             (lambda (pass)
                               (remove-if (lambda (p)
                                            (member p processed-reds
                                                    :test #'equal))
                                          pass))
                             (group-ids-by-pass
                              ;; must remove logical tables and previously
                              ;; processed reduction targets:
                              graph
                              src
                              remsrc-dep<))))))
                        ;; collapsible reductions of src:
                        (collapsible-passes
                         (mapcar (lambda (x y)
                                   y)
                                 nec-passes ult-passes)))
                   (dolist (pass collapsible-passes)
                     (dolist (p pass)
                       (push p processed-reds))
                     (let ((id (gsym 'tabletrans)))
                       (setf (gethash id result-graph)
                             (make-target id
                                          (make-pass-target-expr
                                           target-table
                                           src
                                           pass)))
                       (set-pass-result-targets!
                        result-graph
                        id
                        pass)))))
               ;; process next sources:
               (trans)))))
      (trans))
    ;; unmodified results are already present, so return result-graph
    result-graph))
