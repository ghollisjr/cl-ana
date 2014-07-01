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
      otherwise")))

  (defun find-dependencies (expr token)
    "Descends through expr, finding any forms of the form (token x)
which is then interpreted as a dependency on x (token must be a
symbol)"
    (let ((deps
           (make-hash-table :test 'equal)))
      (labels ((rec (ex)
                 (when (listp ex)
                   (if (and (length-equal ex 2)
                            (eq (first ex)
                                token))
                       (setf (gethash (second ex)
                                      deps)
                             t)
                       (mapcar #'rec ex)))))
        (rec expr)
        (hash-keys deps))))

  (defun make-target (id expr &key
                                val
                                stat)
    (let ((deps (find-dependencies expr 'res))
          (pdeps (find-dependencies expr 'par)))
      (make-instance 'target
                     :id id
                     :expr expr
                     :deps deps
                     :pdeps pdeps
                     :val val
                     :stat stat)))

  (defvar *symbol-tables*
    (make-hash-table :test 'equal)
    "Maps from project id to symbol-table for that project")

  (defvar *target-tables*
    (make-hash-table :test 'equal))

  (defvar *project-id* nil)

  (defvar *transformation-table*
    (make-hash-table :test 'equal))

  (defvar *params-table*
    (make-hash-table :test 'equal))

  (defvar *args-tables*
    (make-hash-table :test 'equal))

  (defun project ()
    "Returns current project"
    *project-id*)

  (defun symbol-table (&optional (project-id *project-id*))
    "Returns symboltable identified by id; defaults to current graph"
    (gethash project-id *symbol-tables*))

  (defun target-table (&optional (project-id *project-id*))
    (gethash project-id *target-tables*))

  ;; makeres utilities:
  (defun pipe-functions (fns input)
    "Evaluates each function in fns given input either from the
  initial input or from the output of the previous function in the
  list fns"
    (let ((val input))
      (dolist (f fns)
        (setf val (funcall f val)))
      val))

  ;; Major function: depsort
  ;;
  ;; this uses lists for sets, inefficient for large dependency
  ;; graphs, so it needs to be updated in the future
  (defun depsort (target-table)
    "Returns list of ids from target-table in the order from least
  dependent to most dependent."
    (let ((depmap (make-hash-table :test 'equal)))
      (labels ((rec (id)
                 ;; returns full list of dependencies for id
                 (let ((deps (target-deps (gethash id target-table))))
                   (when deps
                     (reduce (lambda (ds d)
                               (adjoin d ds :test #'equal))
                             (mapcan #'rec deps)
                             :initial-value deps)))))
        (loop
           for id being the hash-keys in target-table
           do (setf (gethash id depmap)
                    (rec id)))
        (sort (hash-keys target-table)
              (lambda (x y)
                (not (member y (gethash x depmap)
                             :test #'equal)))))))

  ;; Major function: param-dependencies
  ;;
  ;; finds full list of targets which depend on parameter and returns
  ;; list of ids
  (defun param-dependencies (parameter target-table)
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
        deps))))

(defmacro res (id &optional (project-id *project-id*))
  "Expands to whatever the symbol for id in project identified by
project-id is, nil if id or project not specified."
  (awhen (symbol-table project-id)
    (awhen (gethash id it)
      it)))

(defmacro par (id)
  "Outside of generating function, returns the last used value of a
parameter.  Inside, expands to whatever the current parameter value
is."
  `(values
    (gethash ',id
             (gethash *project-id* *args-tables*))))

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
  `',project-id)

(defmacro defpars (params)
  "Adds parameters to project"
  (let ((result
         (set-difference (gethash *project-id* *params-table*)
                         params
                         :key (lambda (x)
                                (if (listp x)
                                    (first x)
                                    x)))))
    (loop for p in params
       do
         (push p result))
    (setf (gethash *project-id* *params-table*)
          result))
  nil)

(defmacro undefpars (params)
  "Undefines parameters in params from project"
  (setf (gethash *project-id* *params-table*)
        (remove-if (lambda (p)
                     (member p params
                             :test #'equal))
                   (gethash *project-id* *params-table*))))

(defmacro defres (id &body body)
  "Defines a result target with id and value expression `(progn ,@body)."
  ;; establish symbol mapping
  (let* ((symtab (symbol-table *project-id*))
         (tartab (target-table *project-id*)))
    ;; set symbol mapping when needed
    (when (not (gethash id symtab))
      (setf (gethash id symtab)
            (gentemp "res" :makeres)))
    ;; add target to table
    (let* ((oldtar (gethash id tartab))
           (val (if oldtar
                    (target-val oldtar)
                    nil))
           (stat (if oldtar
                     (target-stat oldtar)
                     nil)))
      (setf (gethash id tartab)
            (make-target id `(progn ,@body)
                         :val val
                         :stat stat))))
  `',id)

(defun setresfn (id value)
  "Function version of setres"
  (setf (target-val
         (gethash id
                  (gethash *project-id* *target-tables*)))
        value)
  (setf (target-stat
         (gethash id
                  (gethash *project-id* *target-tables*)))
        t))

(defmacro setres (id value)
  "Sets target value of id in project to value and the status to t so
it will not be recomputed."
  `(setresfn ',id ,value))

(defun unsetresfn (id)
  "Function version of unsetres"
  (setf (target-stat
         (gethash id
                  (gethash *project-id* *target-tables*)))
        nil))

(defmacro unsetres (id)
  "Sets status of target to nil, will be recomputed."
  `(unsetresfn ',id))

(defmacro settrans (transforms &key
                                 (op :add)
                                 (project-id *project-id*))
  "Takes each transformation function in list transforms (should be
literal functions, available at compile & load times) and depending on
op does something to the project referred to by project-id.

transforms will not be evaluated and should be a list form containing
the transform functions.

op can be :set or :add for setting the entire list of
transforms or adding to the end of the transform list.

Each transformation is a function taking a target table and returning
a new target table.  The only constraint is that the initial targets
be present in the output table.

Returns full transformation list from project after applying op."
  (case op
    (:add
     (setf (gethash project-id *transformation-table*)
           (nconc (gethash project-id *transformation-table*)
                  transforms)))
    (:set
     (setf (gethash project-id *transformation-table*)
           transforms)))
  `',(gethash project-id *transformation-table*))

(defmacro compres (&optional (project-id *project-id*))
  "Returns a compiled function which will generate result targets
given keyword arguments for each project parameter.  If default values
of parameters are specified, these will be used when no explicit value
is given."
  (let ((symtab (gethash project-id *symbol-tables*))
        (tartab (gethash project-id *target-tables*))
        (fintab
         (let ((fns (gethash project-id *transformation-table*))
               (input (gethash project-id *target-tables*)))
           (if fns
               (pipe-functions fns input)
               input))))
    ;; ensure symbols are defined for fintab
    (loop
       for id being the hash-keys in fintab
       do (let ((sym (gethash id symtab)))
            (when (not sym)
              (setf (gethash id symtab)
                    (gentemp "res" :makeres)))))
    (alexandria:with-gensyms (expr)
      (let* ((sorted-ids
              (depsort fintab))
             (symbindings
              (loop
                 for id in sorted-ids
                 collecting
                   (let ((tar (gethash id fintab)))
                     `(,(gethash id symtab)
                        (if (target-stat
                              (gethash ',id
                                       (gethash ',project-id *target-tables*)))
                            (target-val
                             (gethash ',id
                                      (gethash ',project-id *target-tables*)))
                            ,(aif (gethash id symtab)
                                  `(let ((,expr
                                          ,(target-expr tar)))
                                     (setf (symbol-value ',it)
                                           ,expr
                                           (target-val (gethash ',id ,tartab))
                                           ,expr
                                           (target-stat (gethash ',id ,tartab))
                                           t)
                                     ,expr)
                                  (target-expr tar)))))))
             (lambda-list (gethash project-id *params-table*))
             (params (mapcar (lambda (x)
                               (first (mklist x)))
                             lambda-list))
             (tartab (gethash project-id *target-tables*))
             (argmap (progn
                       (when (not (gethash project-id *args-tables*))
                         (setf (gethash project-id *args-tables*)
                               (make-hash-table :test 'equal)))
                       `(gethash ',project-id *args-tables*)))
             (body
              `(progn
                 ;; Set target values to nil which need updating due to
                 ;; new parameter values
                 ,@(loop
                      for p in params
                      appending
                        `((when (not (equal ,p
                                            (gethash ',p ,argmap)))
                            (setf (gethash ',p ,argmap)
                                  ,p)
                            ,@(loop
                                 for pdep in (param-dependencies p fintab)
                                 appending
                                   `((when (target-stat (gethash ',pdep ,fintab))
                                       (setf (target-stat (gethash ',pdep ,fintab))
                                             nil))
                                     (when (and (gethash ',pdep
                                                         ,tartab)
                                                (target-stat (gethash ',pdep ,tartab)))
                                       (setf (target-stat (gethash ',pdep ,tartab))
                                             nil)))))))
                 
                 ;; execute computations for targets which need
                 ;; updating.
                 (let* ,(cars symbindings)
                   ,@(loop
                        for x in symbindings
                        collect `(setf ,@x))
                   nil)))
             (comp-form
              `(lambda (&key ,@lambda-list)
                 (macrolet ((par (id)
                              id))
                   ,body))))
        `(symbol-function
          (compile (gensym) ,comp-form))))))

(defmacro makeres (&rest args)
  "Macro which compiles and executes generating function given args.
Treat args as if they will be evaluated."
  `(let ((comp (compres)))
     (funcall comp ,@args)))
