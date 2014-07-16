(in-package :makeres-tabletrans)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tabletrans (target-table)
    (makeres::pipe-functions (list #'pass-collapse
                                   #'pass-merge)
                             target-table)))

;;;; New algorithm:

;; Copied operators: table-pass (for internal implementation), dotab,
;; tab, ltab.

;; Should have everything evaluated at compile, load, execute in the end
;;(eval-when (:compile-toplevel :load-toplevel :execute)

(defun table-reduction? (expr)
  "True if expr is a dotab, ltab or tab expression"
  (when expr
    (destructuring-bind (progn &rest forms) expr
      (let ((tab-op (first (first forms))))
        (member tab-op (list 'dotab 'ltab 'tab)
                :test 'eq)))))

(defun table-reduction-source (expr)
  "Returns source for table reduction, nil if expr is not of a
table-reduction."
  (when (table-reduction? expr)
    (cadadr expr)))

(defun immediate-reductions (target-table tab)
  "Returns list of immediately dependent table reductions for a
table"
  (remove-if-not (lambda (id)
                   (let* ((tar (gethash id target-table))
                          (expr (target-expr tar)))
                     (and (table-reduction? expr)
                          (equal (table-reduction-source expr)
                                 (list 'res tab)))))
                 (hash-keys target-table)))

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
         (sorted-ids (sort (chained-reductions target-table src)
                           dep<)))
    (format t "sorted-ids: ~a~%" sorted-ids)
    (when sorted-ids
      (let ((pass (list (pop sorted-ids)))
            (result nil))
        (format t "sorted-ids: ~a~%" sorted-ids)
        (labels ((rec ()
                   (dolist (i sorted-ids)
                     (format t "i: ~a~%" i)
                     (when (every (lambda (p)
                                    (format t "dep< ~a ~a: ~a~%"
                                            i p (funcall dep< i p))
                                    (funcall dep<
                                             i
                                             p))
                                  pass)
                       (format t "pushing ~a~%" i)
                       (push i pass)))
                   (push (reverse pass) result)
                   (print pass)
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

(defun copy-target-table (target-table)
  (let ((result (make-hash-table :test 'equal)))
    (loop
       for id being the hash-keys in target-table
       for tar being the hash-values in target-table
       do (setf (gethash id result)
                (copy-target tar)))
    result))

(defun make-pass-target-expr (graph src pass)
  "Return expression for pass target over src, collapsing all results
from pass up to src."
  )

(defun set-pass-result-targets! (result-graph id pass)
  "Sets result-graph targets from pass so that they make use of the
  returned results for the pass target id.")

(defun tabletrans (target-table)
  "Performs necessary graph transformations for table operators"
  ;; clear gsyms
  (clrgsym 'tabletrans)
  (let* ((graph (copy-target-table target-table))
         (dep< (dep< graph))
         (result-graph
          (copy-target-table target-table))
         ;; list of targets already processed
         (processed nil))
    (labels
        ((trans ()
           (let ((srcs
                  (set-difference
                   (ultimate-source-tables
                    graph
                    processed)
                   processed)))
             ;; (decf *count*)
             (format t "processed: ~a~%" processed)
             (format t "srcs: ~a~%" srcs)
             ;; (when (minusp *count*)
             ;;   (return-from tabletrans nil))
             (when srcs
               (dolist (src srcs)
                 (push src processed)
                 (let* ((imm-reds (immediate-reductions graph src))
                        (imm-red-graph
                         (map->hash-table
                          (cons
                           (cons src (gethash src graph))
                           (remove-if (lambda (cons)
                                        (target-stat
                                         (cdr cons)))
                                      (mapcar (lambda (k)
                                                (cons k (gethash k graph)))
                                              imm-reds)))))
                        ;; immediate passes:
                        (imm-passes
                         (group-ids-by-pass imm-red-graph src))
                        ;; chained reductions for src:
                        (chained
                         (chained-reductions graph src))
                        ;; graph of dependencies relative to ultimate source:
                        (ult-graph
                         (let ((result (make-hash-table :test 'equal)))
                           (dolist (red (set-difference chained processed))
                             (let* ((tar (gethash red graph))
                                    (stat (target-stat tar)))
                               (when (not stat)
                                 (let* ((expr (target-expr tar))
                                        (source (table-reduction-source expr))
                                        (new-target
                                         (copy-target tar)))
                                   (when (and (listp source)
                                              (eq (first source) 'res))
                                     (when (not (equal (second source)
                                                       src))
                                       (setf (target-deps new-target)
                                             (remove (second source)
                                                     (target-deps new-target))))
                                     (setf (gethash red result)
                                           new-target))))))
                           (setf (gethash src result)
                                 (copy-target (gethash src graph)))
                           result))
                        ;; passes relative to ultimate source:
                        (ult-passes
                         (group-ids-by-pass
                          ult-graph
                          src
                          ;; modify this to remove table source from depmap
                          (let ((depmap (make-hash-table :test 'equal)))
                            (labels ((rec (id)
                                       ;; returns full list of dependencies for id
                                       (let ((deps
                                              (copy-list
                                               (target-deps
                                                (gethash id target-table)))))
                                         (when deps
                                           (reduce (lambda (ds d)
                                                     (adjoin d ds :test #'equal))
                                                   (mapcan #'rec deps)
                                                   :initial-value deps)))))
                              (loop
                                 for id being the hash-keys in target-table
                                 do (setf (gethash id depmap)
                                          (rec id)))
                              (lambda (x y)
                                (not (member y (gethash x depmap)
                                             :test #'equal)))))))
                        ;; collapsible reductions of src:
                        (collapsible-passes
                         (mapcar (lambda (x y)
                                   y)
                                 imm-passes ult-passes)))
                   (format t "ult-graph: ~a~%" (map->alist ult-graph))
                   (format t "ult-passes: ~a~%" ult-passes)
                   (format t "imm-passes: ~a~%" imm-passes)
                   (format t "Collapsible-passes: ~a~%" collapsible-passes)
                   (dolist (pass collapsible-passes)
                     (dolist (p pass)
                       (push p processed))
                     (let ((id (gsym 'tabletrans)))
                       (setf (gethash id result-graph)
                             (make-target id
                                          (make-pass-target-expr
                                           src
                                           graph
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
