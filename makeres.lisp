(in-package :makeres)

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

;; makeres is separate from urd (universal result database), analysis
;; package will be built on top of makeres and urd (universal result
;; database)

;;;; May have found a way to implement optimizations: Graph
;;;; transformations.  Instead of using the same dependency graph for
;;;; the entire process, pass graph to transformers which generate new
;;;; graphs.
;;;;
;;;; This pipeline of graphs should culminate in a final graph (the
;;;; temp graph) which has all the initially specified nodes as well
;;;; as any additional nodes required for efficiency; the structure
;;;; can be changed arbitrarily however, and the only requirement is
;;;; that the initial nodes are present.  The computation function
;;;; responsible for making the result will be generated from this
;;;; final dependency graph, and initial results will be collected
;;;; from the temporary graph and stored in the final result graph
;;;; which has the original structure.
;;;;
;;;; E.g., for table reductions, could create nodes which represent
;;;; all immediate dependencies of a table node (node id could be a
;;;; list containing all table dependency ids for a given table).  The
;;;; expression form would then be a loop which at the end returns a
;;;; list of the results needed from the table.
;;;;
;;;; This seems general enough for just about any
;;;; computation/optimization combination for which one would use a
;;;; dependency graph.
;;;;
;;;; There is one concern about recovering from errors.  As written,
;;;; the algorithm is: Generate temp graph -> Compute temp graph
;;;; results -> Copy final results from temp graph.  But, if any nodes
;;;; in the temp graph fail, then the final results may not be copied
;;;; properly.  This could be alleviated by providing a handler with
;;;; an option to copy completed results to the final graph.  This
;;;; would mean that the handler expression would need access to the
;;;; temp graph object, but seems doable.

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
     (stat
      :accessor target-stat
      :initarg :stat
      :initform nil
      :documentation "computation status, nil when needs computing")))
  
  (defvar *symbol-tables*
    (make-hash-table :test 'equal)
    "Maps from project id to symbol-table for that project")

  (defvar *target-tables*
    (make-hash-table :test 'equal))

  (defvar *project-id* nil)

  (defun project ()
    "Returns current project"
    *project-id*)
  
  (defun symbol-table (&optional (project-id *project-id*))
    "Returns symboltable identified by id; defaults to current graph"
    (gethash project-id *symbol-tables*))

  (defun target-table (&optional (project-id *project-id*))
    (gethash project-id *target-tables*))

  (defun find-dependencies (expr)
    "Descends through expr, finding any forms of the form (res x)
which is then interpreted as a dependency on x"
    (let ((deps
           (make-hash-table :test 'equal)))
      (labels ((rec (ex)
                 (when (listp ex)
                   (if (and (length-equal ex 2)
                            (eq (first ex)
                                'res))
                       (setf (gethash (second ex)
                                      deps)
                             t)
                       (mapcar #'rec ex)))))
        (rec expr)
        (hash-keys deps))))
  
  (defun make-target (id expr &optional stat)
    (let ((deps (find-dependencies expr)))
      (make-instance 'target
                     :id id
                     :expr expr
                     :deps deps
                     :stat stat))))

(defmacro res (id &optional (project-id *project-id*))
  "Expands to whatever the symbol for id in project identified by
project-id is, nil if id or project not specified."
  (awhen (symbol-table project-id)
    (awhen (gethash id it)
      it)))

(defmacro in-project (project-id)
  "Selects graph identified by graph-id for use.  Graph does not need
initialization, will be initialized automatically if necessary."
  ;; select graph
  (setf *project-id* project-id)
  ;; initialize target table
  (when (not (gethash project-id *target-tables*))
    (setf (gethash project-id *target-tables*)
          (make-hash-table :test 'equal)))
  ;; initialize symbol table
  (when (not (gethash project-id *symbol-tables*))
    (setf (gethash project-id *symbol-tables*)
          (make-hash-table :test 'equal)))
  nil)

(defmacro defres (id expr &optional (project-id *project-id*))
  "Defines a result target with id and value expression expr."
  ;; establish symbol mapping
  (let* ((symtab (symbol-table project-id))
         (sym (aif (gethash id symtab)
                   it
                   (let ((result (gensym)))
                     ;; set symbol mapping when needed
                     (setf (gethash id symtab)
                           result)
                     result)))
         (tartab (target-table project-id)))
    ;; add target to table
    (let* ((oldtar (gethash id tartab))
           (stat (if oldtar
                     (target-stat oldtar)
                     nil))
           (tar (make-target id expr)))
      (setf (gethash id tartab)
            (make-target id expr stat))))
  `',id)
