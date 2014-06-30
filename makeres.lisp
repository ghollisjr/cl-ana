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
;;;; do something else, i.e. you need a second pass.
;;;;
;;;; So this procedure results in either numbering your passes to keep
;;;; track of how many you have or trying to come up with memorable
;;;; names for each pass.
;;;;
;;;; My work had escalated to the point of combining the two
;;;; approaches, having names with numerical suffixes denoting various
;;;; stages of the analysis, but always wondering if the distinctions
;;;; were really necessary.  I had formalized this system to a degree
;;;; that made me think a computer should be doing the work.
;;;;
;;;; 

;; computations for a graph with different parameters are placed in
;; the same graph.  Provide functions for copying graph to a new
;; graph-id to allow storage of different results.

;; makeres is separate from universal result database,
;; analysis package will be built on top of makeres and urd
;; (universal result database)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *graphs*
    (make-hash-table :test 'equal))
  (defvar *graph-id* nil))

(defmacro use-graph (graph-id)
  "Selects graph identified by graph-id for use.  Graph does not need
initialization, will be initialized automatically if necessary."
  (setf *graph-id* graph-id)
  (when (not (gethash graph-id *graphs*))
    (setf (gethash graph-id *graphs*)
          (make-hash-table :test 'equal)))
  nil)

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

(defmacro defres (id expr)
  "Defines a result with id and value expression expr.  "
  nil)
