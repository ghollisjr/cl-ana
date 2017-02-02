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

(in-package :cl-ana.makeres)

(declaim (optimize (debug 3)))

;;;; makeres is a make-like tool for generating your analysis, i.e. it
;;;; uses a dependency graph to determine what to do in order to
;;;; generate any/all results you desire.  Why couldn't you just use
;;;; GNU make?  Well because for any non trivial work, you'll have to
;;;; specify a bit of how to do what you want, and furthermore since
;;;; the target is von Neumann architecture computers, you'll have to
;;;; provide some optimizations if you want to do things
;;;; declaratively and efficiently.
;;;;
;;;; makeres bridges the gap by allowing you to specify any number of
;;;; graph transformations to put in between the initial dependency
;;;; graph specification and the final
;;;; ready-for-literal-interpretation dependency graph.
;;;;
;;;; My initial (and still rather exemplary) usage concept was for
;;;; analyzing particle physics data.  This consisted of making
;;;; multiple passes over semilarge datasets, collecting various
;;;; results along the way.  The approach almost everyone uses
;;;; starting out is to
;;;;
;;;; * Do everything in a single pass over the entire dataset
;;;;
;;;; But this fails once you need information from the initial pass to
;;;; do something like filtering: you need a second pass.
;;;;
;;;; So this procedure results in either numbering your passes to keep
;;;; track of how many you have or trying to come up with memorable
;;;; names for each pass.  (If you want to get fancy, you can call it
;;;; a pipeline.)
;;;;
;;;; My work had escalated to the point of combining the two
;;;; approaches, having names with numerical suffixes denoting various
;;;; stages of the analysis, but always wondering if the distinctions
;;;; were really necessary.  I had formalized this system to a degree
;;;; that made me think a computer should be doing the work.
;;;;
;;;; Probably due to my experience with make, I found that viewing the
;;;; problem as a series of targets and dependencies made much more
;;;; sense than trying to keep up with passes over data.  I tried many
;;;; approaches in my head before settling on makeres.

;; computations for a graph with different parameters are placed in
;; the same graph.  Provide functions for copying graph to a new
;; graph-id to allow storage of different results.

;;;; There is one concern about recovering from errors.  As written,
;;;; the algorithm is: Generate final graph -> Compute final graph
;;;; results -> Copy results from final graph.  But, if any nodes in
;;;; the temp graph fail, then the final results may not be copied
;;;; properly.  This could be alleviated by providing a handler with
;;;; an option to copy completed results to the final graph.  This
;;;; would mean that the handler expression would need access to the
;;;; final graph object, but seems doable.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; structure containing target information
  (defclass target ()
    ((id
      :accessor target-id
      :initarg :id
      :documentation "Identification")
     (expr
      :accessor target-expr
      :initarg :expr
      :documentation "Expression for computing")
     (deps
      :accessor target-deps
      :initarg :deps
      :documentation "list of target ids which are needed prior")
     (pdeps
      :accessor target-pdeps
      :initarg :pdeps
      :documentation "list of explicit parameter dependencies")
     (val
      :accessor target-val
      :initarg :val
      :initform nil
      :documentation "computation value, nil when needs computing,
      return value whenever computed")
     (stat
      :accessor target-stat
      :initarg :stat
      :initform nil
      :documentation "computation status, nil when needs computing, t
      otherwise")
     (timestamp
      :accessor target-timestamp
      :initarg :timestamp
      :initform nil
      :documentation "Time of most recent computation in seconds")
     (load-stat
      :accessor target-load-stat
      :initarg :load-stat
      :initform nil
      :documentation "T if target is loaded into memory, NIL otherwise")
     ;; save-stat not necessary due to new model: Targets are always
     ;; written to disk immediately after computation.  Therefore the
     ;; only disk status necessary is the load status.
     ;;
     ;; (save-stat
     ;;  :accessor target-save-stat
     ;;  :initarg :save-stat
     ;;  :initform nil
     ;;  :documentation "T if most recent computation has been written to
     ;;  disk, NIL otherwise")
     )))

(defmethod print-object ((tar target) stream)
  (format stream "~S"
          (copy-list
           `(target
             :id ,(target-id tar)
             :expr ,(target-expr tar)
             :deps ,(target-deps tar)
             :pdeps ,(target-pdeps tar)
             :val ,(target-val tar)
             :stat ,(target-stat tar)
             :timestamp ,(target-timestamp tar)
             :load-stat ,(target-load-stat tar)))))

(defun find-dependencies (expr token)
  "Descends through expr, finding any forms of the form (token x)
which is then interpreted as a dependency on x (token must be a
symbol)"
  (let ((deps
         (make-hash-table :test 'equal)))
    (flet ((actual-listp (x)
             (and (listp x)
                  (listp (cdr (last x))))))
      (labels ((rec (ex)
                 ;; check for
                 (when (actual-listp ex)
                   (if (and (length-equal ex 2)
                            (eq (first ex)
                                token))
                       (setf (gethash (second ex)
                                      deps)
                             t)
                       (mapcar #'rec ex)))))
        (rec expr)
        (hash-keys deps)))))

(defun make-target (id expr &key
                              val
                              stat
                              timestamp
                              load-stat)
  (let ((deps (find-dependencies expr 'res))
        (pdeps (find-dependencies expr 'par)))
    (make-instance 'target
                   :id id
                   :expr expr
                   :deps (remove-if (lambda (d)
                                      (equal d id))
                                    deps)
                   :pdeps pdeps
                   :val val
                   :stat stat
                   :timestamp timestamp
                   :load-stat load-stat)))

(defun copy-target (target)
  (make-instance 'target
                 :id (copy-tree (target-id target))
                 :expr (copy-tree (target-expr target))
                 :deps (copy-tree (target-deps target))
                 :pdeps (copy-tree (target-pdeps target))
                 :val (copy-tree (target-val target))
                 :stat (copy-tree (target-stat target))
                 :timestamp (target-timestamp target)
                 :load-stat (target-load-stat target)))

(defparameter *makeres-propogate* nil
  "Set to t if you want dependents of uncomputed results to have
their statuses set to nil")

(defun makeres-set-auto-propogate (stat)
  "nil stat means don't propogate need-to-recompute to dependents of
results needing computation, non-nil stat means do propogate."
  (setf *makeres-propogate* stat))

(defvar *compiled-generators*
  (make-hash-table :test 'equal)
  "Maps from project id to a gensym used for the compiled function
name generated by compres.")

(defvar *target-tables*
  (make-hash-table :test 'equal))

(defvar *fin-target-tables*
  (make-hash-table :test 'equal))

(defvar *project-id* nil)

(defvar *transformation-table*
  (make-hash-table :test 'equal))

(defvar *params-table*
  (make-hash-table :test 'equal))

(defvar *makeres-args*
  (make-hash-table :test 'equal)
  "Map from arg symbol to supplied or default value at makeres
    execution")

(defun compres-fname (&optional project-id)
  "Returns gensym for project used for compres"
  (symbol-macrolet ((gsym (gethash project-id *compiled-generators*)))
    (if gsym
        gsym
        (setf gsym (gensym)))))

(defun project ()
  "Returns current project"
  *project-id*)

(defun project-parameters ()
  (mapcar #'first
          (gethash (project) *params-table*)))

(defun project-targets ()
  (hash-keys (target-table)))

(defun target-table (&optional (project-id *project-id*))
  (gethash project-id *target-tables*))

(defun copy-target-table (target-table)
  "Returns a new hash-table object with copies of each target from
target-table."
  (let ((result (make-hash-table :test 'equal)))
    (loop
       for id being the hash-keys in target-table
       for tar being the hash-values in target-table
       do (setf (gethash id result)
                (copy-target tar)))
    result))

;; makeres utilities:
(defun pipe-functions (fns input)
  "Evaluates each function in fns given input either from the
  initial input or from the output of the previous function in the
  list fns"
  (let ((val (copy-target-table input)))
    (dolist (f fns)
      (setf val (funcall f val)))
    val))

;; Major function: res-dependencies
;;
;; returns full list of dependencies on a target
(defun res-dependents (res-id target-table)
  "Returns full list of result-dependent targets in target-table"
  (let* (;; table mapping from id to those targets immediately
         ;; dependent on id
         (dependent-table
          (make-hash-table :test 'equal))
         (explicit-deps
          (loop
             for id being the hash-keys in target-table
             for tar being the hash-values in target-table
             when (member res-id (target-deps tar)
                          :test #'equal)
             collecting id))
         (deps nil))
    ;; fill dependent-table
    (loop
       for id being the hash-keys in target-table
       for target being the hash-values in target-table
       do
         (loop
            for d in (target-deps target)
            do (push id
                     (gethash d dependent-table))))
    ;; collect all dependents of explicit-deps and insert into deps
    (labels ((rec (id)
               (setf deps
                     (adjoin id deps
                             :test #'equal))
               (mapcar #'rec (gethash id dependent-table))))
      (mapcar #'rec explicit-deps)
      deps)))

;; Major function: dep<
;;
;; this uses lists for sets, inefficient for large dependency
;; graphs, so it needs to be updated in the future

(defun res-dependencies (graph id)
  (labels ((rec (id)
             ;; returns full list of dependencies for id
             (let ((tar (gethash id graph)))
               (when tar
                 (let ((deps (copy-list (target-deps tar))))
                   (when deps
                     (reduce (lambda (ds d)
                               (adjoin d ds :test #'equal))
                             (mapcan #'rec deps)
                             :initial-value deps)))))))
    (rec id)))

(defun depmap (target-table)
  "Returns the dependency map for a target-table.  Used by dep< and
can be useful for generating the explicit dependency graph for a
target table."
  (let ((depmap (make-hash-table :test 'equal)))
    (memolet ((rec (id)
                   ;; returns full list of dependencies for id
                   (let ((tar (gethash id target-table)))
                     (when tar
                       (let ((deps (copy-list (target-deps tar))))
                         (when deps
                           (reduce (lambda (ds d)
                                     (adjoin d ds :test #'equal))
                                   (mapcan (lambda (i)
                                             (copy-list (rec i)))
                                           deps)
                                   :initial-value deps)))))))
      (loop
         for id being the hash-keys in target-table
         do (setf (gethash id depmap)
                  (rec id)))
      depmap)))


(defun dep< (target-table)
  "Returns comparison function from target-table which returns true
when the left argument does not depend on the right argument."
  (let ((depmap (depmap target-table)))
    (lambda (x y)
      (not (member y (gethash x depmap)
                   :test #'equal)))))

;; Topological sort functions
;;
;; This is the replacement for the inefficient topological sorting
;; algorithm I called depsort only because I had never heard the term
;; topological sorting and had never read an actual algorithm.  As
;; usual, there are much better algorithms than the one I whipped up
;; in a few minutes.

(defun target-table-edge-map (&optional (target-table (target-table)))
  "Returns an edge map from the target-table.  This returns an
inverted dependency graph, with children mapping to parents instead of
parents mapping to children.  Therefore, the result of this needs to
be inverted before passing to topological sorting."
  (let ((result (make-hash-table :test 'equal)))
    (loop
       for id being the hash-keys in target-table
       do (setf (gethash id result)
                (target-deps (gethash id target-table))))
    result))

(defun edge-map-ids (edge-map)
  (let ((edge-map (map->alist edge-map))
        (traversed (make-hash-table :test 'equal)))
    (loop
       for n in edge-map
       do (destructuring-bind (p . cs) n
            (when (not (gethash p traversed))
              (setf (gethash p traversed) t))
            (loop
               for c in cs
               do
                 (when (not (gethash c traversed))
                   (setf (gethash c traversed) t)))))
    (hash-keys traversed)))

(defun decompress-edge-map (edge-map)
  "Generates an alist of single-dependency edges, the standard
academic representation of a DAG"
  (let ((edge-map (map->hash-table edge-map 'equal))
        (decompressed nil))
    (loop
       for parent being the hash-keys in edge-map
       do
         (let ((children (gethash parent edge-map)))
           (if children
               (loop
                  for c in children
                  do (push (cons parent c)
                           decompressed))
               (push (list parent)
                     decompressed))))
    decompressed))

(defun compress-edge-map (edge-map-alist)
  "Generates a hash-table of compressed edges, i.e. all direct
children listed for each parent."
  (let ((result (make-hash-table :test 'equal)))
    (loop
       for (p . c) in edge-map-alist
       do
         (if (null c)
             (when (not (gethash p result))
               (setf (gethash p result)
                     nil))
             (push c (gethash p result))))
    result))

(defun edge-map-node-set (edge-map)
  "Returns a hash-table acting as the set of nodes referenced in
edge-map."
  (let ((result (make-hash-table :test 'equal)))
    (flet ((insert (x)
             (when (not (gethash x result))
               (setf (gethash x result)
                     t))))
      (loop
         for id being the hash-keys in edge-map
         for deps being the hash-values in edge-map
         do
           (insert id)
           (loop for d in deps
              do (insert d))))
    result))

(defun invert-edge-map (edge-map)
  "Inverts an edge-map.  edge-map represents the edges in a directed
  acyclic graph with keys being the parent nodes and values being
  lists of all immediate child nodes."
  (let* ((edge-map (map->hash-table edge-map 'equal))
         ;; original node set
         (edge-map-nodes (edge-map-node-set edge-map))
         (nodes (hash-keys edge-map))
         (decompressed (decompress-edge-map edge-map))
         (inverted
          (loop
             for cons in decompressed
             when (cdr cons)
             collecting (cons (cdr cons)
                              (car cons))

             ;; Old version
             ;; collecting
             ;; (if (cdr cons)
             ;;     (cons (cdr cons)
             ;;           (car cons))
             ;;     (list (car cons)))
               ))
         (compressed (compress-edge-map inverted))
         (result-nodes (edge-map-node-set compressed)))
    ;; Add uncaptured nodes back into the result as singletons
    (loop
       for k being the hash-keys in edge-map-nodes
       when (not (gethash k result-nodes))
       do (setf (gethash k compressed)
                nil))
    compressed))

(defun topological-sort (edge-map)
  "Topologically sorts an edge-map which represents a directed acyclic
graph.  edge-map should be a datatype that has a method
map->hash-table defined.  The keys should be parent nodes, and the
values should be lists of child nodes.

Since makeres uses the reverse scheme, instead listing nodes as keys
and their parents as value lists, invert-edge-map needs to be used on
target-table edge-maps before topologically sorting the values."
  (let* ((edge-map (map->hash-table edge-map 'equal))
         ;; Traversed nodes
         (tset (make-hash-table :test 'equal))
         ;; List of all nodes
         (nodes (edge-map-ids edge-map))
         ;; Sorted ids
         (sorted nil))
    ;; Algorithm is very simple
    ;;
    ;; 1. Start with the first node in remaining
    ;; 2. If node is already in tset, do nothing
    ;; 3. Else, place node into the list of traversed nodes
    ;; 4. If node has children, then loop over children
    ;; 5. After looping over children, place node at the top of
    ;;    sorted and pop out of remaining
    ;; 6. Continue over remaining until no more nodes are in remaining
    (labels ((traverse (node)
               (when (not (gethash node tset))
                 (setf (gethash node tset)
                       t)
                 (let* ((children (gethash node edge-map)))
                   (when children
                     (mapcar #'traverse children))
                   (push node sorted)))))
      (mapcar #'traverse nodes)
      sorted)))

;; depsort-graph functions:

;; Here, dep< means "does not depend".  So, (dep< x y) being T means
;; that x does not depend on y.  It's like a less-than relationship
;; because sorting using "does not depend" results in a list sorted
;; from least dependent to most dependent.
(defun last-dep (x lst dep<)
  "Returns last dependency of x found in lst and t; if no
dependencies are found then returns two nil values"
  (let ((res
         (remove-if (lambda (d)
                      (funcall dep< x d))
                    lst)))
    (if res
        (values (first (last res))
                t)
        (values nil nil))))

(defun insert-after! (insertion lst token)
  "Inserts insertion into list immediately following the first
element #'equal to token."
  (if (null lst)
      (push insertion lst)
      (if (equal (first lst) token)
          (setf (cdr lst)
                (cons insertion
                      (cdr lst)))
          (insert-after! insertion (cdr lst) token))))

(defun depsort (ids dep<)
  "Sorts a list of values given dependency comparison function dep<;
special algorithm since sort needs transitive operator while dep< is
not transitive in general."
  (let ((result nil))
    (loop
       for i in ids
       do (multiple-value-bind (last-dep any)
              (last-dep i result dep<)
            (if any
                (insert-after! i result last-dep)
                (push i result))))
    result))

;; old depsort graph algorithm
(defun depsort-graph-old (target-table &optional dep<)
  "Returns dependency-sorted target ids from target-table, if dep<
is provided then it is used instead of the dep< computed from the
target-table."
  (let ((dep< (if dep<
                  dep<
                  (dep< target-table))))
    (depsort (hash-keys target-table)
             dep<)))

(defun depsort-graph (target-table)
  "Returns dependency-sorted target ids from target-table.  Uses the
new topological sort algorithm."
  (topological-sort
   (invert-edge-map
    (target-table-edge-map target-table))))

;; Major function: param-dependents
;;
;; finds full list of targets which depend on parameter and returns
;; list of ids
(defun param-dependents (parameter target-table)
  "Returns full list of parameter-dependent targets in target-table"
  (let* (;; table mapping from id to those targets immediately
         ;; dependent on id
         (dependent-table
          (make-hash-table :test 'equal))
         (explicit-deps
          (loop
             for id being the hash-keys in target-table
             for tar being the hash-values in target-table
             when (member parameter (target-pdeps tar)
                          :test #'equal)
             collecting id))
         (deps nil))
    ;; fill dependent-table
    (loop
       for id being the hash-keys in target-table
       for target being the hash-values in target-table
       do
         (loop
            for d in (target-deps target)
            do (push id
                     (gethash d dependent-table))))
    ;; collect all dependents of explicit-deps and insert into deps
    (labels ((rec (id)
               (setf deps
                     (adjoin id deps
                             :test #'equal))
               (mapcar #'rec (gethash id dependent-table))))
      (mapcar #'rec explicit-deps)
      deps)))

;;; Caching

(defparameter *cache-table*
  (make-hash-table :test 'equal)
  "Map from project id to cache function")

(defun defcache (cache-fn)
  "Sets the caching function.  cache-fn should be a function which,
when given a single target id, manages the cache according to its
strategy and leaves the target table in a state such that the result
for the given target id is ready to be accessed from the
target table.

Default behavior is unlimited cache, i.e. all results remain in the
target table."
  (setf (gethash (project) *cache-table*)
        cache-fn))

(defun cacheres (id)
  (when (and (or (gethash id (target-table))
                 (gethash id
                          (gethash *project-id*
                                   *fin-target-tables*)))
             (not (ignored? id)))
    (let ((cache-fn (gethash (project) *cache-table*)))
      (if cache-fn
          (funcall cache-fn id)
          (open-cache id)))))

;;; Various caching strategies:

(defun open-cache (id)
  "Caching which assumes infinite cache size"
  (symbol-macrolet ((tar (gethash id (target-table)))
                    (fintar (gethash id
                                     (gethash *project-id*
                                              *fin-target-tables*))))
    (let ((load-stat
           (target-load-stat
            (or tar fintar))))
      (when (not load-stat)
        (load-target id)))))

(defun singleton-cache (id)
  "Caching which only allows a single target to be loaded at a time.
Minimal memory use, maximal strain on hard drive."
  (let* ((fintab (gethash *project-id* *fin-target-tables*)))
    (symbol-macrolet ((tar (gethash id (target-table)))
                      (fintar (gethash id
                                       (gethash *project-id*
                                                *fin-target-tables*))))
      (let ((load-stat
             (cond
               (tar (target-load-stat tar))
               (fintar (target-load-stat fintar)))))
        (when (not load-stat)
          ;; Unload target table ids
          (loop
             for unload-id being the hash-keys in (target-table)
             for unload-tar being the hash-values in (target-table)
             do (unload-target id))
          ;; Unload final target table ids
          ;;
          ;; Technically double-unloads target table ids, but it
          ;; doesn't cost much time.
          (loop
             for unload-id being the hash-keys in fintab
             for unload-tar being the hash-values in fintab
             do (unload-target unload-id))
          (load-target id))))))

(defun fixed-cache (size)
  "Returns a caching function which limits the number of in-memory
targets to size."
  (let ((cache (make-array size :initial-element nil))
        (i 0))
    (lambda (id)
      (symbol-macrolet ((tar (gethash id (target-table)))
                        (fintar (gethash id
                                         (gethash *project-id*
                                                  *fin-target-tables*))))
        (let ((load-stat (target-load-stat
                          (or tar
                              fintar))))
          (when (not load-stat)
            ;; this technically excludes NIL from being a target id
            (when (aref cache i)
              (unload-target (aref cache i)))
            (load-target id)
            (setf (aref cache i) id)
            (setf i
                  (mod (+ i 1)
                       size))))))))

;; Caching utility functions:

(defun unload-target (id)
  (symbol-macrolet ((tar
                     (gethash id (target-table)))
                    (fintar
                     (gethash id
                              (gethash *project-id*
                                       *fin-target-tables*))))
    (cond
      (tar
       (setf (target-val tar) nil)
       (setf (target-load-stat tar) nil)
       (setf (target-val fintar) nil)
       (setf (target-load-stat fintar) nil))
      (fintar
       (setf (target-val fintar) nil)
       (setf (target-load-stat fintar) nil)))))

;; Result retrieval

(defmacro res (id)
  "Expands to whatever the symbol for id in project identified by
project-id is, nil if id or project not specified."
  `(resfn ',id))

(defun resfn (id &optional (project-id *project-id*))
  (cacheres id)
  (multiple-value-bind (tar tarstat)
      (gethash id (gethash project-id *target-tables*))
    (if tarstat
        (target-val tar)
        (multiple-value-bind (fintab fintabstat)
            (gethash project-id *fin-target-tables*)
          (if fintabstat
              (target-val
               (gethash id
                        fintab))
              (error "target ~a does not exist" id))))))

(defmacro par (id)
  "Outside of generating function, returns the last used value of a
parameter.  Inside, expands to whatever the current parameter value
is."
  `(values
    (gethash ',id
             (gethash *project-id* *makeres-args*))))

(defun parfn (id)
  (values (gethash id (gethash *project-id* *makeres-args*))))

;;;; Additional transformation-induced dependencies

;; defunct
(defparameter *trans->added-deps-fn*
  (make-hash-table :test 'eq))

;; defunct
(defmacro deftransdeps (trans fn)
  "Assigns function for finding additional transformation-induced
dependencies for a given target in a graph.  Transformation-induced
means any dependencies not found by searching the target expression
directly for (res ...) forms.

trans should be a graph transformation function.

fn should be a function accepting one argument, a target graph, and
returning a modified graph with each target having sufficiently many
additional dependencies induced by the transformation to allow
propogration via makeres-propogate!."
  `(setf (gethash ,trans *trans->added-deps-fn*)
         ,fn))

(defparameter *trans->propogator-fn*
  (make-hash-table :test 'eq))

(defmacro defpropogator (trans fn)
  "Assigns function for finding additional transformation-induced
dependencies for a given target in a graph.  Transformation-induced
means any dependencies not found by searching the target expression
directly for (res ...) forms.

trans should be a graph transformation function.

fn should be a function accepting one argument, a target graph, and
returning a modified graph with each target having sufficiently many
additional dependencies induced by the transformation to allow
propogration via makeres-propogate!, as well as having the target
status for each target be appropriate for makeres-propogate!, as the
result graph is what will be checked, not the original target table."
  `(setf (gethash ,trans *trans->propogator-fn*)
         ,fn))

;; Project creation macro
;;
;; Parameters should be:
;; * Project ID
;; * Project path
;; * Transformations
;; * Caching strategy
(defmacro defproject (id path transformations cache-strategy
                      &key
                        warnings-p
                        ;; Unless something changes, functions can't
                        ;; be logged
                        (ignore-functions-p t))
  "Defines a makeres project.  id is unevaluated whereas all other
arguments are evaluated.

warnings-p controls whether warnings are printed during compilation.

ignore-functions-p should be set to T unless you find a way to log
functions."
  `(progn
     (in-project ,id)
     (set-project-path ,path)
     (settrans ,transformations :op :set)
     (defcache ,cache-strategy)
     (setf *makeres-warnings* ,warnings-p)
     ;; This doesn't work due to being called before any targets are
     ;; defined.  The user needs to call load-project after loading
     ;; his/her project definition.
     ;;
     ;; Initialize target-stat for each logged result
     ;; (init-logged-stats)
     (when ,ignore-functions-p
       (logres-ignore-by #'function-target?))))

(defmacro in-project (project-id)
  "Selects graph identified by graph-id for use.  Graph does not need
initialization, will be initialized automatically if necessary."
  `(in-project-fn ',project-id))

(defun in-project-fn (project-id)
  (setf *project-id* project-id)
  ;; initialize target table
  (when (not (gethash project-id *target-tables*))
    (setf (gethash project-id *target-tables*)
          (make-hash-table :test 'equal)))
  ;; initialize final target table
  (when (not (gethash project-id *fin-target-tables*))
    (setf (gethash project-id *fin-target-tables*)
          (make-hash-table :test 'equal)))
  project-id)

(defmacro defpars (params)
  "Adds parameters to project, updating default values for existing
parameters."
  (alexandria:with-gensyms (result ps)
    `(let* ((,ps ',params)
            (,result
             (set-difference (gethash *project-id* *params-table*)
                             ,ps
                             :key (lambda (x)
                                    (if (listp x)
                                        (first x)
                                        x)))))
       (loop for p in ,ps
          do
            (push p ,result))
       (setf (gethash *project-id* *params-table*)
             ,result)
       nil)))

(defmacro undefpars (&rest params)
  "Undefines parameters in params from project"
  (alexandria:with-gensyms (ps)
    `(let ((,ps ',params))
       (setf (gethash *project-id* *params-table*)
             (remove-if (lambda (p)
                          (member p ,ps
                                  :test #'equal))
                        (gethash *project-id* *params-table*))))))

(defmacro defres (id &body body)
  "Defines a result target with id and value expression `(progn ,@body)."
  ;; establish symbol mapping
  (alexandria:with-gensyms (tartab oldtar val stat)
    `(let* ((,tartab (target-table *project-id*))
            (,oldtar (gethash ',id ,tartab))
            (,val (if ,oldtar
                      (target-val ,oldtar)
                      nil))
            ;; statuses are reset
            (,stat nil))
       (setf (gethash ',id ,tartab)
             (make-target ',id `(progn ,@',body)
                          :val ,val
                          :stat ,stat))
       ',id)))

(defun defresfn (id expr)
  "Defines a result target with id and expression"
  (let* ((tartab (target-table *project-id*))
         (oldtar (gethash id tartab))
         (val (if oldtar
                  (target-val oldtar)
                  nil))
         ;; statuses are reset
         (stat nil))
    (setf (gethash id tartab)
          (make-target id (if (or (not (listp expr))
                                  (not (eq (first expr) 'progn)))
                              `(progn ,expr)
                              expr)
                       :val val
                       :stat stat))
    id))

(defun defresfn-uniq (id expr)
  "Defines a result target with id and expression"
  (let* ((tartab (target-table *project-id*))
         (oldtar (gethash id tartab))
         (val (if oldtar
                  (target-val oldtar)
                  nil))
         ;; statuses are reset
         (stat nil))
    (setf (gethash id tartab)
          (make-target id (if (or (not (listp expr))
                                  (not (eq (first expr) 'progn)))
                              `(progn ,expr)
                              expr)
                       :val val
                       :stat stat))
    id))

(defmacro defres-uniq (id &body body)
  "Only defines the target if the expression would be different from
that already in the target table."
  (alexandria:with-gensyms (bod)
    `(let ((,bod `(progn ,@',body)))
       (when (or (not (gethash ',id (target-table)))
                 (not (equal (target-expr
                              (gethash ',id (target-table)))
                             ,bod)))
         (defres ,id ,@body)))))

(defmacro undefres (&rest res)
  "Undefines result targets"
  `(progn
     ,@(loop
          for r in res
          collecting
            `(progn
               (remhash ',r
                        (gethash *project-id* *target-tables*))
               (remhash ',r
                        (gethash *project-id* *fin-target-tables*))))
     nil))

(defun setresfn (id value &optional timestamp)
  "Function version of setres.  Only sets value in target-table if id
is present.  Creates new target in final target table if one is not
present."
  ;; *target-tables*:
  (when (gethash id
                 (target-table))
    (setf (target-stat
           (gethash id
                    (target-table)))
          t)
    (setf (target-timestamp
           (gethash id
                    (target-table)))
          (if timestamp
              timestamp
              (get-universal-time)))
    (setf (target-val
           (gethash id
                    (target-table)))
          value)
    ;; Caching
    (save-target id)
    (cacheres id))
  ;; *fin-target-tables*:
  (when (gethash id
                 (gethash *project-id* *fin-target-tables*))
    (setf (target-stat
           (gethash id
                    (gethash *project-id* *fin-target-tables*)))
          t)
    (when (gethash id
                   (gethash *project-id* *target-tables*))
      (setf (target-timestamp
             (gethash id
                      (gethash *project-id* *fin-target-tables*)))
            (target-timestamp
             (gethash id
                      (gethash *project-id* *target-tables*)))))
    (setf (target-val
           (gethash id
                    (gethash *project-id* *fin-target-tables*)))
          value)
    ;; Caching
    (when (not (gethash id (target-table)))
      (save-target id)
      (cacheres id))))

(defmacro setres (id value)
  "Sets target value of id in project to value and the status to t so
it will not be recomputed."
  `(setresfn ',id ,value))

(defun unsetresfn (id)
  "Function version of unsetres"
  (setf (target-stat
         (gethash id
                  (gethash *project-id* *target-tables*)))
        nil)
  (when (gethash id
                 (gethash *project-id* *fin-target-tables*))
    (setf (target-stat
           (gethash id
                    (gethash *project-id* *fin-target-tables*)))
          nil)))

(defmacro unsetres (id)
  "Sets status of target to nil, will be recomputed."
  `(unsetresfn ',id))

(defun clrresfn ()
  "Function version of clrres"
  (let ((tartab (gethash *project-id* *target-tables*))
        (fintab (gethash *project-id* *fin-target-tables*)))
    (loop
       for k being the hash-keys in tartab
       do (setf (target-stat (gethash k tartab))
                nil))
    (loop
       for k being the hash-keys in fintab
       do (setf (target-stat (gethash k fintab))
                nil))))

(defmacro clrres ()
  "Clears all status for result targets in current project"
  `(clrresfn))

(defun settrans (transforms &key
                              (op :add))
  "Takes each transformation function in list transforms (should be
names of functions available at compile & load times) and depending on
op does something to the project referred to by project-id.

transforms will not be evaluated and should be a list form containing
the transform functions.

op can be :set or :add for setting the entire list of transforms or
adding to the front of the transform list.  (Front-adding seemed more
reasonable since this allows languages to be built on top of previous
layers.)

Each transformation is a function taking a target table and returning
a new target table.  The only constraint is that the initial targets
be present in the output table.

Returns full transformation list from project after applying op."
  (let ((pid (project)))
    (case op
      (:add
       (setf (gethash pid *transformation-table*)
             (append transforms
                     (gethash pid *transformation-table*))))
      (:set
       (setf (gethash pid *transformation-table*)
             transforms)))
    (gethash pid *transformation-table*)))

(defparameter *makeres-warnings* t
  "Set to nil if you want to suppress warnings from compilation")

(defun compseq (forms)
  "Compiles a sequence of forms sequentially"
  (loop
     for i from 1
     for f in forms
     collecting
       (symbol-function
        (compile 'compseqfn `(lambda () ,f)))))

(defun makeres-forms (fintab &optional (project-id *project-id*))
  "Returns the individual target forms for the makeres computation"
  ;; ensure symbols are defined for fintab
  (alexandria:with-gensyms (val)
    (let* ((sorted-ids
            (depsort-graph fintab))
           (set-exprs
            (progn
              (loop
                 for id in sorted-ids
                 when (not (target-stat
                            (gethash id
                                     (gethash project-id
                                              *fin-target-tables*))))
                 collecting
                   (let ((tar (gethash id fintab)))
                     `(let ((,val
                             ,(target-expr tar)))
                        (setresfn ',id ,val))))
              )))
      set-exprs)))

(defun makeres-form (fintab)
  "Returns the form for a function which performs the makeres
computation"
  ;; ensure symbols are defined for fintab
  (alexandria:with-gensyms (val)
    (let* ((sorted-ids
            (depsort-graph fintab))
           (set-exprs
            (progn
              (loop
                 for id in sorted-ids
                 when (not (target-stat
                            (gethash id
                                     (gethash project-id
                                              *fin-target-tables*))))
                 collecting
                   (let ((tar (gethash id fintab)))
                     `(let ((,val
                             ,(target-expr tar)))
                        (setresfn ',id ,val))))
              ))
           (lambda-list (gethash project-id *params-table*))
           (params (mapcar (lambda (x)
                             (first (mklist x)))
                           lambda-list))
           (body
            (progn
              `(progn
                 ;; Save supplied parameter values:
                 ,@(loop
                      for p in params
                      appending
                        `((when (not
                                 (equal ,p
                                        (gethash ',p
                                                 (gethash (project) *makeres-args*))))
                            (setf (gethash ',p
                                           (gethash (project) *makeres-args*))
                                  ,p))))
                 ;; execute computations for targets which need
                 ;; updating.
                 ,@set-exprs
                 nil)))
           (comp-form
            `(lambda (&key ,@lambda-list)
               (macrolet ((par (id)
                            id))
                 ,body))))
      comp-form)))

(defun transform-target-table (&optional (project-id *project-id*))
  "Returns the fully transformed target table by passing it through
the transformation pipeline."
  (let ((fns
         (gethash project-id *transformation-table*))
        (input (copy-target-table
                (gethash project-id *target-tables*))))
    (if fns
        (pipe-functions fns input)
        input)))

(defun compres (&optional (project-id *project-id*))
  "Returns a compiled function which will generate result targets
given keyword arguments for each project parameter.  If default values
of parameters are specified, these will be used when no explicit value
is given."
  (let ((fintab
         (transform-target-table project-id)))
    ;; Update fintab:
    (setf (gethash project-id *fin-target-tables*)
          fintab)
    ;; ensure symbols are defined for fintab
    (let ((target-fns
           (if *makeres-warnings*
               (compseq (makeres-forms fintab))
               (suppress-output
                 (compseq (makeres-forms fintab))))))
      (lambda (&rest args)
        (loop
           for fn in target-fns
           do (funcall fn))))))

;; Old version of compres.  Compiles a single form, but this started
;; to cost an insane amount of memory.  SBCL doesn't handle the large
;; form very well, so now each target form gets its own function, and
;; the functions are called in sequence.
(defun compres-old (&optional (project-id *project-id*))
  "Returns a compiled function which will generate result targets
given keyword arguments for each project parameter.  If default values
of parameters are specified, these will be used when no explicit value
is given."
  (let ((fintab
         (transform-target-table project-id)))
    ;; Update fintab:
    (setf (gethash project-id *fin-target-tables*)
          fintab)
    ;; ensure symbols are defined for fintab
    (let ((comp-form
           (makeres-form fintab)))
      (symbol-function
       (if *makeres-warnings*
           (compile (compres-fname project-id) comp-form)
           (suppress-output
             (compile (compres-fname project-id) comp-form)))))))

;; Defunct
(defun added-dep-graph (graph)
  (let* ((trans-list
          (gethash (project)
                   *transformation-table*))
         (added-fns
          (remove nil
                  (mapcar (lambda (x)
                            (gethash x *trans->added-deps-fn*))
                          trans-list))))
    (pipe-functions added-fns graph)))

(defun transforms-propogate (graph)
  "Propogates as makeres-propogate! would but for special cases which
graph transformations must individually manage."
  (let* ((trans-list
          (gethash (project)
                   *transformation-table*))
         (propogators
          (remove nil
                  (mapcar (lambda (x)
                            (gethash x *trans->propogator-fn*))
                          trans-list))))
    (pipe-functions propogators graph)))

(defun makeres-propogate! ()
  (let ((graph (transforms-propogate (target-table))))
    (loop
       for id being the hash-keys in graph
       for tar being the hash-values in graph
       do (when (null (target-stat tar))
            (loop
               for r in (res-dependents id graph)
               when (gethash r (target-table))
               do (unsetresfn r))))))

(defvar *sticky-pars*
  t
  "Set to nil if you don't want default values to be updated by new
  args to makeres")

(defun makres-set-sticky-pars (stat)
  (setf *sticky-pars* stat)
  "Sets sticky parameter switch; non-nil means new values given to
  makeres are used for the new default value.")

(defvar *proj->par->def-last?*
  (make-hash-table :test #'equal)
  "map from project to parameter to whether the last value given was
  the default value or not")

(defun par-def-last? (par)
  (gethash par
           (gethash *project-id* *proj->par->def-last?*)))

(defun par-set-last? (par)
  (second (multiple-value-list
           (gethash par
                    (gethash *project-id*
                             *makeres-args*)))))

(defun par-empty-last? (par)
  "Returns true if parameter was neither default last time nor had an
explicit value set"
  (not (or (par-def-last? par)
           (par-set-last? par))))

(defun assert-par-def-last! (par def-last-p)
  "Assert that parameter's value was default last time"
  (setf (gethash par
                 (gethash *project-id* *proj->par->def-last?*))
        def-last-p))

(defun par-needs-updating? (par args)
  "Returns true if parameter par needs updating given the argument
list args"
  (let ((pkeysym (keywordify par))
        (arg-map
         (map->hash-table
          (plist->alist args)
          'eq)))
    (multiple-value-bind (argval argstat)
        (gethash pkeysym arg-map)
      (multiple-value-bind (lastval laststat)
          (gethash par
                   (gethash *project-id*
                            *makeres-args*))
        (if (par-empty-last? par)
            t
            (if *sticky-pars*
                ;; sticky case
                argstat
                ;; non-sticky case
                (if (par-def-last? par)
                    argstat
                    ;; par-set-last?
                    (or (null argstat)
                        (not (equal argval lastval))))))))))

(defun computation-stat-path ()
  "Returns the path for the computation status file"
  (let ((path
         (make-pathname :name "stat"
                        :directory (pathname-directory
                                    (current-path)))))
    (ensure-directories-exist path)))

(defun makeres ()
  "Function which compiles and executes generating function."
  (let ((all-args nil))
    (when (not (gethash *project-id* *makeres-args*))
      (setf (gethash *project-id* *makeres-args*)
            (make-hash-table :test 'eq)))
    (when (not (gethash *project-id* *proj->par->def-last?*))
      (setf (gethash *project-id* *proj->par->def-last?*)
            (make-hash-table :test 'eq)))
    ;; argument parsing:
    (let* ((targets (if (keywordp (first all-args))
                        (project-targets)
                        (first all-args)))
           (args (if (keywordp (first all-args))
                     all-args
                     (rest all-args))))
      ;; unset any results dependent on new parameter values
      (let* ((params
              (gethash *project-id* *params-table*))

             (arg-map
              (map->hash-table (plist->alist args)
                               'eq)))
        (loop
           for p in params
           do (let ((psym (first (mklist p))))
                (multiple-value-bind (val stat)
                    (gethash (keywordify psym)
                             arg-map)

                  (multiple-value-bind (oldval oldstat)
                      (gethash psym
                               (gethash *project-id* *makeres-args*))
                    (when (par-needs-updating? psym args)
                      (when stat
                        (setf (gethash psym
                                       (gethash *project-id* *makeres-args*))
                              val))
                      (let ((pdeps (param-dependents
                                    psym
                                    (gethash *project-id*
                                             *target-tables*))))
                        (loop
                           for pdep in pdeps
                           do
                             (progn
                               (unsetresfn pdep)))))
                    ;; remove from stored table if not specified and not sticky:
                    (when (and (not *sticky-pars*)
                               (null stat))
                      (remhash psym
                               (gethash *project-id* *makeres-args*))))))))
      ;; When sticky pars update args:
      (when *sticky-pars*
        (loop
           for psym being the hash-keys in (gethash *project-id*
                                                    *makeres-args*)
           do
             (when (second
                    (multiple-value-list
                     (gethash psym
                              (gethash *project-id*
                                       *makeres-args*))))
               (setf (getf args (keywordify psym))
                     (gethash psym
                              (gethash *project-id*
                                       *makeres-args*))))))
      ;; Perform sanity check on the target table:
      (when (not (checkres))
        (error "Target table fails checkres test"))
      ;; Whenever *makeres-propogate* is non-nil, unset any results
      ;; dependent on null-stat results
      (when *makeres-propogate*
        (makeres-propogate!))

      (let ((comp (compres)))
        ;; Write computation status file:
        (let ((*print-pretty* nil))
          (with-open-file (stat-file (computation-stat-path)
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
            (let ((timestamp (get-universal-time))
                  (to-compute (loop
                                 for id being the hash-keys
                                 in (target-table)
                                 for tar being the hash-values
                                 in (target-table)
                                 when (null (target-stat tar))
                                 collecting id)))
              (format stat-file "~a~%~a~%"
                      timestamp
                      to-compute))))
        (apply comp args)
        (delete-file (computation-stat-path))
        ;; NOTE: I'm not sure if tables which are in the final target
        ;; table which get unloaded and then reloaded will cause
        ;; memory leaks.  In the future, it might be necessary to
        
        ;; Prune the targets not present in the target table
        ;;
        ;; NOTE: In the future, it might be nicer to just remove final
        ;; targets not present in the target table instead of pruning
        ;; the whole log, but right now this is a good substitute
        (pruneres t)))))

;;;; Utilities:

(defun target-ids (&optional (filter #'identity))
  "Returns list of IDs for defined targets in project.  filter should
be a function taking an ID as an argument and returning T for IDs
which should be shown."
  (remove-if-not filter
                 (hash-keys (gethash *project-id* *target-tables*))))

(defun fin-target-ids ()
  "Returns list of ids for defined targets in final target table."
  (hash-keys (gethash *project-id* *fin-target-tables*)))

;; Could create print representation of targets, that way there could
;; just be targets and fin-targets as functions which would return all
;; project targets and final target table targets.

;; Inspects target table to see if there are spurious references to
;; targets

(defun checkres (&optional target-table)
  (let ((target-table
         (if target-table
             target-table
             (target-table)))
        (*print-pretty* nil)
        (result t))
    (loop
       for id being the hash-keys in target-table
       for tar being the hash-values in target-table
       do (loop
             for dep in (target-deps tar)
             do (when (not (gethash dep target-table))
                  (setf result nil)
                  (format t "~s: ~s not present in target-table~%"
                          id dep))))
    result))

;; Deletes target logs for targets which are not present in the
;; target-table.

(defun pruneres (&optional delete-p)
  (let* ((logged-paths
          (mapcar #'pathname
                  (directory
                   (merge-pathnames "**"
                                    (merge-pathnames "targets/"
                                                     (current-path))))))
         (logged-id-strings
          (mapcar (lambda (pn)
                    (read-from-string
                     (first (last (pathname-directory pn)))))
                  logged-paths))
         (*print-pretty* nil))
    (loop
       for logged-id in logged-id-strings
       for logged-path in logged-paths
       do (when (or (not (gethash logged-id (target-table)))
                    (ignored? logged-id))
            (if delete-p
                (progn
                  (format t "Deleting ~a~%"
                          logged-id)
                  (sb-ext:delete-directory logged-path
                                           :recursive t))
                (format t "~a not tracked~%"
                        logged-id))))))

(defun purgeres (&optional delete-p)
  "Finds targets which have null statuses and optionally deletes their
logs."
  (let ((null-stats
         (loop
            for id being the hash-keys in (target-table)
            for tar being the hash-values in (target-table)
            when (and (not (target-stat tar))
                      (probe-file (target-path id)))
            collecting id)))
    (if delete-p
        (loop
           for id in null-stats
           do (progn
                (format t "Deleting ~s~%" id)
                (sb-ext:delete-directory (target-path id)
                                         :recursive t)))
        (loop
           for id in null-stats
           do (format t "~s~%" id)))))

(defun printres (&optional (filter (constantly t)))
  "Prints the target-table with optional filter function which takes
two arguments (id and value) to determine which targets to print."
  (let ((map (mapcar (lambda (cons)
                       (cons (car cons)
                             (resfn (car cons))))
                     (map->alist (target-table)))))
    (format t "Target Table:~%")
    (loop
       for cons in map
       when (funcall filter (car cons) (cdr cons))
       do (format t "~a: ~a~%"
                  (car cons) (cdr cons)))))

;; Utility functions used by mvres:
(defun replace-id (old new form)
  "Replaces (res old) with (res new) wherever it occurs in a form."
  (subst `(res ,new) `(res ,old) form
         :test #'equal))

(defun replace-log-id (old new path)
  (let* ((*print-pretty* nil)
         (oldform
          (read-from-string
           (with-open-file (file path
                                 :direction :input)
             (read file))))
         (newform (replace-id old new oldform)))
    ;; write new form:
    (with-open-file (file path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file "~s~%"
              (with-output-to-string (s)
                (format s "~s" newform))))))

(defun mvres (old-id new-id
              &key quiet-p)
  "Moves a target from old-id to new-id in the target-table,
final-target-table, the logged target path, and in any existing logged
forms.  This process is potentially irreversible, so backups may be
expedient.

Set quiet-p to non-NIL to disable progress messages."
  ;; Check if old-id is in the target-table:
  (when (gethash old-id (target-table))
    ;; Move in target-table
    (let* ((tar (gethash old-id (target-table))))
      ;; Move log
      (when (not (ignored? old-id))
        (when (not quiet-p)
          (format t "Moving log...~%"))
        (rename-file (target-path old-id)
                     (target-path new-id)))
      (when (not quiet-p)
        (format t "Moving in target-table...~%"))
      ;; delete hash-table entry
      (remhash old-id (target-table))
      ;; modify target
      (setf (target-id tar)
            new-id)
      ;; Add new target to table:
      (setf (gethash new-id (target-table))
            tar))
    ;; Modify all affected targets:
    (loop
       for id being the hash-keys in (target-table)
       do (symbol-macrolet ((tar
                             (gethash id (target-table))))
            (when (member old-id (target-deps tar) :test #'equal)
              (when (not quiet-p)
                (format t "Modifying ~a~%" id))
              (setf (target-deps tar)
                    (mapcar (lambda (i)
                              (if (equal i old-id)
                                  new-id
                                  i))
                            (target-deps tar)))
              (when (not (ignored? id))
                (replace-log-id old-id new-id
                                (target-path id "form")))
              (setf (target-expr tar)
                    (replace-id old-id new-id
                                (target-expr tar))))))
    ;; Move in final-target-table
    (symbol-macrolet ((fintab (gethash (project) *fin-target-tables*)))
      (when (gethash old-id fintab)
        (when (not quiet-p)
          (format t "Moving in \"fintab\"...~%"))
        (let* ((tar (gethash old-id fintab)))
          ;; delete hash-table entry
          (remhash old-id fintab)
          ;; modify target
          (setf (target-id tar)
                new-id)
          ;; Add new target to table:
          (setf (gethash new-id fintab)
                tar))
        ;; Modify all affected targets:
        (loop
           for id being the hash-keys in fintab
           do (symbol-macrolet ((tar
                                 (gethash id fintab)))
                (when (member old-id (target-deps tar) :test #'equal)
                  (when (not quiet-p)
                    (format t "Modifying ~a~%" id))
                  (setf (target-deps tar)
                        (mapcar (lambda (i)
                                  (if (equal i old-id)
                                      new-id
                                      i))
                                (target-deps tar)))
                  (setf (target-expr tar)
                        (replace-id old-id new-id
                                    (target-expr tar))))))))))

(defun unsetdepsfn (id
                    &key quiet-p)
  "Unsets all dependencies of id in the target-table.  Set quiet-p to
non-NIL to disable progress messages."
  (when (gethash id (target-table))
    (let ((graph (transforms-propogate (target-table))))
      (loop for dep in (res-dependents id graph)

         do
           (when (not quiet-p)
             (format t "Unsetting ~s~%" dep))
           (unsetresfn dep)))))

(defmacro unsetdeps (id &key quiet-p)
  "Macro version of unsetresfn.  id is unevaluated, quiet-p is."
  `(unsetdepsfn ',id :quiet-p ,quiet-p))
