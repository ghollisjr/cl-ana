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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Very useful functions, adding to package external symbols
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
        (list 'res expr))))

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

;; Needs to have its memo map reset periodically to free space.  Best
;; practice is to place the reset at the top of your transformation if
;; you use this function.
(defun-memoized immediate-reductions (target-table
                                      tab
                                      &key
                                      (reduction-test-fn
                                       #'table-reduction?)
                                      (reduction-source-fn
                                       #'table-reduction-source))
  "Returns list of immediately dependent table reductions for a
table"
  (remove-if-not (lambda (id)
                   (let* ((tar (gethash id target-table))
                          (expr (target-expr tar)))
                     (and (funcall reduction-test-fn expr)
                          (equal (unres
                                  (funcall reduction-source-fn
                                           expr))
                                 tab))))
                 (hash-keys target-table)))

;; Testing this new ltab-chains function
(defun ltab-chains
    (target-table chain-edge-map src
     &key
       (ltab-test-fn #'ltab?)
       (dotab-test-fn #'dotab?))
  "Returns all ltab chains stemming from src.  A ltab chain is simply
a list of ltab ids which are chained reductions of either ltabs or the
source table with id src along with the first non-ltab id which is a
reduction of the last ltab in the chain."
  (labels ((chains (s &optional context)
             (let* ((children (gethash s chain-edge-map))
                    (filtered-children
                     ;; Only accept dotabs and ltabs
                     (remove-if-not
                      (lambda (id)
                        (and (gethash id target-table)
                             (let ((expr (target-expr
                                          (gethash id target-table))))
                               (or (funcall dotab-test-fn expr)
                                   (funcall ltab-test-fn expr)))))
                      children)))
               (if filtered-children
                   (apply #'append
                          (mapcar (lambda (c)
                                    (chains c (cons s context)))
                                  filtered-children))
                   (list (cons s context))))))
    (mapcar #'reverse (chains src))))

(defun ltab-chained-reductions (target-table ltab-chain-edge-map src
                                &key
                                  (ltab-test-fn #'ltab?)
                                  (dotab-test-fn #'dotab?))
  "Returns all reductions directly from ltab chains stemming from src"
  (mapcar #'alexandria:last-elt
          (ltab-chains target-table ltab-chain-edge-map src
                       :ltab-test-fn ltab-test-fn
                       :dotab-test-fn dotab-test-fn)))

(defun necessary-pass-reductions
    (target-table chain-edge-map tab
     &key
       (ltab-test-fn #'ltab?)
       (dotab-test-fn #'dotab?)
       (reduction-test-fn
        #'table-reduction?)
       (reduction-source-fn
        #'table-reduction-source))
  "Returns list of reductions of a table which must be computed via a
pass over the table; equivalent to the union set of all immediate
non-ltab reductions and any reductions chained directly to tab via
logical tables."
  (list->set
   (append (remove-if (lambda (id)
                        (funcall ltab-test-fn
                                 (target-expr
                                  (gethash id target-table))))
                      (immediate-reductions
                       target-table tab
                       :reduction-test-fn reduction-test-fn
                       :reduction-source-fn reduction-source-fn))
           (ltab-chained-reductions target-table
                                    chain-edge-map
                                    tab
                                    :ltab-test-fn ltab-test-fn
                                    :dotab-test-fn dotab-test-fn))
   #'equal))

(defun chained-edge-map (target-table
                         &key
                           (reduction-test-fn #'table-reduction?)
                           (reduction-source-fn
                            #'table-reduction-source))
  "Returns an edgemap which only describes the chains of table
reductions."
  (let* ((uncompressed nil))
    ;; Generate uncompressed table dependency edges
    (loop
       for id being the hash-keys in target-table
       for tar being the hash-values in target-table
       when (funcall reduction-test-fn
                     (target-expr tar))
       do
         (push (cons (unres (funcall reduction-source-fn
                                     (target-expr tar)))
                     id)
               uncompressed))
    (compress-edge-map uncompressed)))

(defun chained-reductions (chained-edge-map src)
  "Returns list of ids for targets from target-table which are
connected via a chain of reductions from src."
  (let ((imm-reds (gethash src chained-edge-map)))
    (when imm-reds
      (append imm-reds
              (mapcan (lambda (red)
                        (copy-list
                         (chained-reductions chained-edge-map
                                             red)))
                      imm-reds)))))

(defun group-ids-by-pass (chained-edge-map src
                          depsorted-ids
                          dep<
                          &key
                            (test (constantly t)))
  "Groups all ids from target-table according the the pass required
over src, optionally using the dependency checker dep< and keeping
only targets for which test returns t."
  (let* ((chained (chained-reductions chained-edge-map src))
         (sorted-ids
          (let ((depsorted depsorted-ids))
            (remove-if-not (lambda (x)
                             (and (member x chained :test #'equal)
                                  (funcall test x)))
                           depsorted))))
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

(defun ultimate-source-tables (target-table
                               &key
                                 ignore
                                 (reduction-test-fn
                                  #'table-reduction?)
                                 (reduction-source-fn
                                  #'table-reduction-source))
  "Returns list of source table ids which are not table reductions of
non-ignored sources."
  (let ((srcs nil)
        (reds nil))
    (loop
       for id being the hash-keys in target-table
       for tar being the hash-values in target-table
       do (let ((expr (target-expr tar)))
            (when (funcall reduction-test-fn expr)
              (let* ((raw-src (funcall reduction-source-fn expr))
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
    (set-difference srcs reds :test #'equal)))

;; Need to modify the removed-*-dep< functions due to the new
;; topological sort algorithm added to makeres.  The concepts of these
;; still apply, but they must be implemented in a new way.
;;
;; As a quick-fix, the depmap needs to be inverted, i.e., instead of
;; finding dependencies, you find dependents.  Then this inverted map
;; can be returned as a compressed version of the edges of a directed
;; acyclic graph.

;; I'm trying a new version of the removed-*-depmap algorithms that I
;; think will be much clearer.
;;
;; The concept is this:
;;
;; When merging reductions, the chain of source tables must not be
;; considered dependencies of the reductions.
;;
;; When merging reductions through logical tables, the contiguous
;; chain of logical tables back to the first non-logical table must
;; not be considered dependencies of the reductions.  The source table
;; itself can and likely should remain a dependency however.
;;
;; Each reduction has a single chain of tables sources, so I should
;;
;; 1. Calculate the full dependency map of the relevant subsection of
;;    the target table
;;
;; 2. Calculate the source table chains for the relevant subsection of
;;    the target table
;;
;; 3. Remove whatever subset of the table chain from the dependency
;;    map for each reduction

(defun chainmap (target-table
                 &key
                   (reduction-test-fn #'table-reduction?)
                   (reduction-source-fn #'table-reduction-source))
  "Returns a hash table mapping from reduction to the chain of source
tables producing the reduction."
  (memolet (;; Returns full list of sources up the chain
            (sources (id &optional context)
                     (let* ((tar (gethash id target-table))
                            (expr (target-expr tar)))
                       (if (funcall reduction-test-fn expr)
                           (let ((src (unres (funcall reduction-source-fn expr))))
                             (sources src (cons src context)))
                           context))))
    (let ((result (make-hash-table :test 'equal)))
      (loop
         for id being the hash-keys in target-table
         for tar being the hash-values in target-table
         when (funcall reduction-test-fn (target-expr tar))
         do (setf (gethash id result)
                  (sources id)))
      result)))

(defun ltab-chainmap (target-table chainmap
                      &key
                        (ltab-test-fn #'ltab?))
  "Returns the ltab chains for each reduction in the target-table.  An
ltab-chain is the chain of tables from the reduction to the nearest
non-logical table source."
  (let ((result (make-hash-table :test 'equal)))
    (loop
       for red being the hash-keys in chainmap
       do (let* ((chain (gethash red chainmap))
                 (spos (position-if-not
                        (lambda (i)
                          (funcall ltab-test-fn
                                   (target-expr
                                    (gethash i target-table))))
                        chain
                        :from-end t))
                 (lchain (subseq chain spos)))
            (setf (gethash red result)
                  lchain)))
    result))

;; I'm trying this without the non-reductions in the table for now
(defun removed-source-depmap (depmap chainmap)
  "Returns a new depmap which does not contain the table sources in
the depmap."
  (let ((result (make-hash-table :test 'equal)))
    (loop
       for id being the hash-keys in depmap
       do (let* ((deps (gethash id depmap))
                 (chain (gethash id chainmap)))
            (when chain
              (setf (gethash id result)
                    (set-difference deps chain
                                    :test #'equal)))))
    result))

(defun removed-source-dep< (depmap)
  (lambda (x y)
    (not (member y (gethash x depmap)
                 :test #'equal))))

(defun removed-ltab-source-depmap (depmap lchainmap)
  (let ((result (make-hash-table :test 'equal)))
    (loop
       for k being the hash-keys in depmap
       do (let* ((deps (gethash k depmap))
                 (lchain (gethash k lchainmap))
                 (ltabs (rest lchain)))
            (when lchain
              (setf (gethash k result)
                    (set-difference deps ltabs :test #'equal)))))
    result))

(defun removed-ltab-source-dep< (depmap)
  (lambda (x y)
    (not (member y (gethash x depmap)
                 :test #'equal))))

;; Inverting chainmaps to yield maps from the source to the full
;; chain:

(defun invert-chainmap (chainmap)
  "Returns a map from the source table to the list of chains
associated with that table."
  (let ((result (make-hash-table :test 'equal)))
    (loop
       for red being the hash-keys in chainmap
       do (let* ((chain (gethash red chainmap))
                 (src (first chain)))
            (push chain
                  (gethash src result))))
    result))

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
  "Returns the list of all argument lists given to all instances of
push-fields in the form which are not within a macrolet definition."
  (when (and form
             (listp form))
    (cond
      ((eq (first form)
           'push-fields)
       (list (copy-list (rest form))))
      ((eq (first form)
           'macrolet)
       (mapcan #'find-push-fields (rest (rest form))))
      (t
       (append (find-push-fields (car form))
               (find-push-fields (cdr form)))))))

(defun replace-push-fields (form replacements)
  "Replaces push-fields within form with replacement as long as it's
not inside a macrolet definition"
  (let ((replacements (copy-tree replacements)))
    (labels ((rec (frm)
               (if (and frm
                        (listp frm))
                   (cond
                     ((eq (first frm)
                          'push-fields)
                      (pop replacements))
                     ((eq (first frm)
                          'macrolet)
                      `(macrolet ,(second frm)
                         ,@(mapcar #'rec (rest (rest frm)))))
                     (t
                      (cons (rec (first frm))
                            (rec (rest frm)))))
                   frm)))
      (rec form))))

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
           (nodes
            (remove src
                    (list->set
                     (labels
                         ((rec (n)
                            (cons (node-id n)
                                  (append
                                   (node-content n)
                                   (mapcan #'rec
                                           (node-children n))))))
                       (rec context-tree)))))
           (reductions
            (remove src
                    (list->set
                     (labels
                         ((rec (n)
                            (let ((subcontent
                                   (append
                                    (node-content n)
                                    (mapcan #'rec
                                            (node-children n)))))
                              (if (tab? (target-expr
                                         (gethash (node-id n)
                                                  graph)))
                                  subcontent
                                  (cons (node-id n)
                                        subcontent)))))
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
           for r in nodes
           do (progn
                (setht reduction->initsym->gsym r)
                (setht reduction->initsym->expr r)
                (setht reduction->return r))))
      ;; Make maps from initsyms to gsyms and expressions for each
      ;; reduction (reduction->initsym->gsym,
      ;; reduction->initsym->expr) as well as map from reduction to
      ;; return form
      (loop
         for r in nodes
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
                            ;; push-field-bindings is a list of the
                            ;; different bindings as found via
                            ;; find-push-fields in the table pass body
                            (push-field-bindings-list
                             (cond
                               ((and (tab? expr)
                                     (not (equal c src)))
                                (find-push-fields (gethash c tab-expanded-expr)))
                               ((ltab? expr)
                                (find-push-fields (table-reduction-body expr)))
                               ;; Source table special case:
                               (t (list nil))))
                            (push-field-syms-list
                             (mapcar #'cars push-field-bindings-list))
                            (push-field-gsyms-list
                             (loop
                                for bs in push-field-syms-list
                                collecting
                                  (loop
                                     for b in bs
                                     collecting (gsym 'tabletrans))))
                            ;; list of maps, one per push-fields form
                            (push-field->gsym-list
                             (loop
                                for push-field-syms in push-field-syms-list
                                for push-field-gsyms in push-field-gsyms-list
                                collecting
                                  (let ((ht (make-hash-table :test 'eq)))
                                    (loop
                                       for sym in push-field-syms
                                       for gsym in push-field-gsyms
                                       do (setf (gethash sym ht)
                                                gsym))
                                    ht)))
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
                               (loop
                                  for push-field-syms in push-field-syms-list
                                  for push-field->gsym in push-field->gsym-list
                                  appending
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
                                     lfields))))
                            (olet-field-bindings-list
                             (loop
                                for push-field-bindings in push-field-bindings-list
                                collecting (append push-field-bindings lfield-bindings)))
                            (olet-field-gsyms-list
                             (loop
                                for push-field-gsyms in push-field-gsyms-list
                                collecting
                                  (append
                                   push-field-gsyms
                                   lfield-gsyms)))
                            (children-exprs
                             (when (node-children node)
                               (mapcar #'rec
                                       (node-children node))))
                            (content-tab->push-field-vector
                             (let ((result (make-hash-table :test 'equal)))
                               (loop
                                  for content in (node-content node)
                                  do
                                    (setf (gethash content result)
                                          (map
                                           'vector
                                           #'identity
                                           (loop
                                              for push-field->gsym
                                              in push-field->gsym-list
                                              for push-fields
                                              in
                                                (find-push-fields
                                                 (table-reduction-body
                                                  (gethash content tab-expanded-expr)))
                                              collecting
                                                (loop
                                                   for (field binding)
                                                   in push-fields
                                                   collecting
                                                     (list field
                                                           (gethash field
                                                                    push-field->gsym)))))))
                               result))
                            (sub-bodies
                             (loop
                                for olet-field-bindings in olet-field-bindings-list
                                for olet-field-gsyms in olet-field-gsyms-list
                                for content-index from 0
                                collect
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
                                                       ,@(aref
                                                          (gethash id content-tab->push-field-vector)
                                                          content-index)))
                                                    (table-reduction-body expr)))))
                                          (node-content node))
                                         children-exprs)
                                        :test #'equal)))))
                       (if (and (not (equal c src))
                                (table-reduction? expr))
                           (let ((result
                                  (replace-push-fields
                                   `(progn
                                      (symbol-macrolet
                                          ,(awhen (gethash c reduction->initsym->gsym)
                                             (loop
                                                for s being the hash-keys in it
                                                for gsym being the hash-values in it
                                                collecting (list s gsym)))
                                        ,@(table-reduction-body
                                           (if (tab? expr)
                                               (gethash c tab-expanded-expr)
                                               expr))))
                                   sub-bodies)))
                             result)
                           (first sub-bodies)))))
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
                      (when ,nrows-var
                        (format t "Event ~a, ~$% complete~%"
                                ,row-var
                                (* 1f2
                                   (/ (float ,row-var)
                                      (float ,nrows-var))))))
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
  ;; Reset memo maps:
  (reset-memo-map #'immediate-reductions)
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
  ;; save lfield definitions
  (save-lfields)
  (let* ((graph (copy-target-table target-table))

         ;; chained reduction and ltab chain edge maps
         (chained-edge-map
          (chained-edge-map graph
                            :reduction-test-fn #'table-reduction?
                            :reduction-source-fn
                            #'table-reduction-source))
         ;; Complete dependency map
         (depmap (depmap graph))
         ;; Chain maps:
         (chainmap (chainmap graph))
         (lchainmap (ltab-chainmap graph chainmap
                                   :ltab-test-fn #'ltab?))
         ;; Removed source depmaps
         (remsrc-depmap
          (removed-source-depmap depmap chainmap))
         (remsrc-dep<
          (removed-source-dep<
           remsrc-depmap))
         ;; (remsrc-depsorted-ids (depsort-graph graph remsrc-dep<))
         (remsrc-depsorted-ids
          (topological-sort
           (invert-edge-map
            remsrc-depmap)))

         ;; special dep< which only adds ltabs sources as dependencies
         ;; when used somewhere other than as the source additionally.
         (remltab-depmap
          (removed-ltab-source-depmap depmap lchainmap))
         (remltab-dep<
          (removed-ltab-source-dep< remltab-depmap))
         ;; (remltab-depsorted-ids (depsort-graph graph remltab-dep<))
         (remltab-depsorted-ids
          (topological-sort
           (invert-edge-map
            remltab-depmap)))

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
                   (ultimate-source-tables
                    graph
                    :ignore processed-srcs)
                   processed-srcs)))
             (when srcs
               (dolist (src srcs)
                 (push src processed-srcs)
                 (let ((ltabs
                        (list->set
                         (alexandria:flatten
                          (mapcar #'butlast
                                  (mapcar #'rest
                                          (ltab-chains
                                           graph
                                           chained-edge-map
                                           src
                                           :ltab-test-fn #'ltab?
                                           :dotab-test-fn #'dotab?)))))))
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
                            graph chained-edge-map src
                            :dotab-test-fn #'dotab?
                            :ltab-test-fn #'ltab?
                            :reduction-test-fn #'table-reduction?
                            :reduction-source-fn #'table-reduction-source))))
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
                             chained-edge-map
                             src
                             remltab-depsorted-ids
                             remltab-dep<
                             :test
                             (lambda (i)
                               (not
                                (or (not (member i nec-reds :test #'equal))
                                    (ltab? (target-expr (gethash i graph)))
                                    (target-stat (gethash i graph))))))))))
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
                              chained-edge-map
                              src
                              remsrc-depsorted-ids
                              remsrc-dep<
                              :test
                              (lambda (i)
                                (not
                                 (or (member i processed-reds :test #'equal)
                                     (ltab? (target-expr
                                             (gethash i graph)))
                                     (target-stat (gethash i graph)))))))))))
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

;; BUGGY VERSION
;;
;; This version attempts to recurse through the lfield maps, but this
;; is completely unnecessary once the table-pass expression is
;; available.  All that needs to be done is to find the (res ...)
;; dependencies in the table-pass body, and add any additional
;; dependencies to the deps list.
;;
;; (defun lfield-dependencies (graph id)
;;   "Returns full list of dependencies caused by lfields"
;;   (labels ((lfield-deps (id)
;;              ;; Returns the res dependencies directly imposed by
;;              ;; reference to an lfield from a table reduction
;;              (when (and (not (target-stat (gethash id graph)))
;;                         (table-reduction?
;;                          (target-expr (gethash id graph))))
;;                (let* ((tar (gethash id graph))
;;                       (expr (target-expr tar))
;;                       (src (unres (table-reduction-source expr)))
;;                       (lfields
;;                        (gethash src
;;                                 (gethash (project)
;;                                          *proj->tab->lfields*)))
;;                       (lfield-map
;;                        (let ((result (make-hash-table :test 'eq)))
;;                          (loop
;;                             for lfield in lfields
;;                             do (setf (gethash (first lfield) result)
;;                                      `(progn ,@(rest lfield))))
;;                          result)))
;;                  (labels ((rec (lfield)
;;                             ;; finds all lfields referred to by lfield
;;                             ;; expression including the lfield itself
;;                             (let* ((expr (gethash lfield lfield-map))
;;                                    (referred
;;                                     (remove-if-not
;;                                      (lambda (field)
;;                                        (gethash field lfield-map))
;;                                      (cl-ana.makeres::find-dependencies
;;                                       expr
;;                                       'field))))
;;                               (when referred
;;                                 (append referred
;;                                         (mapcan #'rec referred))))))
;;                    (let* ((lfield-deps
;;                            (list->set
;;                             (mapcan
;;                              #'rec
;;                              (remove-if-not
;;                               (lambda (field)
;;                                 (gethash field lfield-map))
;;                               (cl-ana.makeres::find-dependencies
;;                                expr
;;                                'field)))
;;                             #'eq))
;;                           (lfield-dep-merged-expr
;;                            `(progn
;;                               ,@(loop
;;                                    for ld in lfield-deps
;;                                    collecting (gethash ld lfield-map))))
;;                           (lfield-res-deps
;;                            (cl-ana.makeres::find-dependencies
;;                             lfield-dep-merged-expr
;;                             'res)))
;;                      (remove-if
;;                       (lambda (i)
;;                         (target-stat (gethash i graph)))
;;                       (list->set
;;                        lfield-res-deps
;;                        #'equal)))))))
;;            (lfdrec (id)
;;              ;; Finds lfield dependencies to an id both directly and
;;              ;; from any possible dependency path
;;              (when (not (target-stat (gethash id graph)))
;;                (let ((imm-deps
;;                       (append (lfield-deps id)
;;                               (target-deps (gethash id graph)))))
;;                  (list->set
;;                   (append imm-deps
;;                           (mapcan #'lfdrec
;;                                   imm-deps))
;;                   #'equal)))))
;;     (lfdrec id)))

;;; Propogation:

;; Propogation strategy: Create new targets for each lfield and set
;; its status based on whether it has been changed.
(defpropogator #'tabletrans
    (lambda (graph)
      (let ((graph (copy-target-table graph))
            (changed-lfields
             (changed-lfields))
            (tab-lfield->gsym (make-hash-table :test 'equal)))
        ;; Create tab-lfield->gsym map
        (loop
           for tab being the hash-keys
           in (gethash (project) *proj->tab->lfields*)
           for lfields being the hash-values
           in (gethash (project) *proj->tab->lfields*)
           do (loop
                 for lf in lfields
                 do (setf (gethash (list tab (car lf))
                                   tab-lfield->gsym)
                          (gsym 'tabletrans))))

        ;; Create lfield targets
        (loop
           for tab-lfield being the hash-keys in tab-lfield->gsym
           for gsym being the hash-values in tab-lfield->gsym
           do
             (destructuring-bind (tab lfield) tab-lfield
               (let* ((lfields (gethash tab
                                        (gethash (project)
                                                 *proj->tab->lfields*)))
                      (lfield-map
                       (let ((result (make-hash-table :test 'eq)))
                         (loop
                            for lfield in lfields
                            do (setf (gethash (first lfield) result)
                                     `(progn ,@(rest lfield))))
                         result)))
                 (labels ((lfield-deps (lfield)
                            ;; finds all lfields referred to by lfield
                            ;; expression including the lfield itself
                            (let* ((expr (gethash lfield lfield-map))
                                   (referred
                                    (remove-if-not (lambda (field)
                                                     (gethash field lfield-map))
                                                   (cl-ana.makeres::find-dependencies
                                                    expr
                                                    'field))))
                              (when referred
                                (append referred
                                        (mapcan #'lfield-deps
                                                referred))))))
                   (setf (gethash gsym graph)
                         (make-target
                          gsym
                          `(progn
                             ',lfield
                             ,@(loop
                                  for lfdep
                                  in (lfield-deps lfield)
                                  collecting `(res ,(gethash (list tab lfdep)
                                                             tab-lfield->gsym))))
                          :stat
                          ;; (not (not x)) is an easy way of only getting
                          ;; T or NIL from x
                          (not
                           (not (not (member lfield
                                             (gethash tab
                                                      changed-lfields)
                                             :test #'eq))))))))))
        (loop
           for id being the hash-keys in graph
           for tar being the hash-values in graph
           when (table-reduction? (target-expr tar))
           do (let* ((expr (target-expr tar))
                     (src (unres (table-reduction-source expr)))
                     (lfields
                      (gethash src
                               (gethash (project)
                                        *proj->tab->lfields*)))
                     (lfield-map
                      (let ((result (make-hash-table :test 'eq)))
                        (loop
                           for lfield in lfields
                           do (setf (gethash (first lfield) result)
                                    `(progn ,@(rest lfield))))
                        result)))
                (labels ((lfield-deps (lfield)
                           ;; finds all lfields referred to by lfield
                           ;; expression including the lfield itself
                           (let* ((expr (gethash lfield lfield-map))
                                  (referred
                                   (remove-if-not (lambda (field)
                                                    (gethash field lfield-map))
                                                  (cl-ana.makeres::find-dependencies
                                                   expr
                                                   'field))))
                             (when referred
                               (append referred
                                       (mapcan (lambda (x)
                                                 (lfield-deps x))
                                               referred))))))
                  (let* ((lfield-deps
                          (list->set
                           (mapcan
                            (lambda (x)
                              (cons x (lfield-deps x)))
                            (remove-if-not (lambda (field)
                                             (gethash field lfield-map))
                                           (cl-ana.makeres::find-dependencies
                                            expr
                                            'field)))
                           #'eq))
                         (lfield-dep-merged-expr
                          `(progn ,@(loop
                                       for ld in lfield-deps
                                       collecting (gethash ld lfield-map))))
                         (lfield-res-deps
                          (append
                           (mapcar (lambda (lf)
                                     (gethash (list src lf)
                                              tab-lfield->gsym))
                                   lfield-deps)
                           (cl-ana.makeres::find-dependencies
                            lfield-dep-merged-expr
                            'res))))
                    (symbol-macrolet ((deps
                                       (target-deps tar)))
                      (setf deps
                            (list->set (append deps
                                               lfield-res-deps)
                                       #'equal)))))))
        graph)))

;; Logging lfields

(defun lfield-log-path ()
  (merge-pathnames "makeres-table/lfield-log"
                   (current-path)))

(defun save-lfields ()
  "Saves the current lfield definitions for the project to disk"
  (let* ((path (lfield-log-path))
         (tab->lfields
          (gethash (project) *proj->tab->lfields*))
         (*print-pretty* nil))
    (ensure-directories-exist path)
    (with-open-file (file path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (loop
         for tab being the hash-keys in tab->lfields
         for lfields being the hash-values in tab->lfields
         do (format file "~s ~s~%"
                    tab lfields)))))

(defun load-lfields ()
  "Returns hash-table mapping table to lfield definitions stored on
disk"
  (let* ((path (lfield-log-path))
         (result-alist nil))
    (with-open-file (file path
                          :direction :input
                          :if-does-not-exist nil)
      (when file
        (do ((line (read-line file nil nil)
                   (read-line file nil nil)))
            ((null line))
          (with-input-from-string (s line)
            (push (cons (read s)
                        (read s))
                  result-alist))))
      (cl-ana.map:map->hash-table result-alist 'equal))))

(defun changed-lfields ()
  "Returns lfields which are different from those logged.  Result is a
hash-table mapping from table to a list of changed lfield symbols."
  (when (not (gethash (project) *proj->tab->lfields*))
    (setf (gethash (project) *proj->tab->lfields*)
          (make-hash-table :test 'equal)))
  (let* ((tab->lfields (gethash (project) *proj->tab->lfields*))
         (tab->logged-lfields
          (load-lfields))
         (result nil))
    (loop
       for tab being the hash-keys in tab->lfields
       do (let* ((logged (gethash tab tab->logged-lfields))
                 (logged-ht
                  (cl-ana.map:map->hash-table logged 'equal))
                 (current (gethash tab tab->lfields))
                 (current-ht
                  (cl-ana.map:map->hash-table current 'equal))
                 (res nil))
            (loop
               for lf being the hash-keys in current-ht
               do (let ((defcurrent (gethash lf current-ht))
                        (deflogged (gethash lf logged-ht)))
                    (when (not (equal defcurrent deflogged))
                      (push lf res))))
            (when res
              (push (cons tab (nreverse res))
                    result))))
    (cl-ana.map:map->hash-table result 'equal)))
