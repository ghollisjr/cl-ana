(in-package :makeres)

;;;; Demo of language:

;;; Must use/select a project before using makeres

;; Select project
(in-project test)
;; project ID can be any lisp form

;; Define parameters for project
(defpars ((source (list 1 2 3 4 5 6 7))
          (scale 1)))
;; Each parameter form will be used in a keyword lambda-list, so you
;; can provide default values if you like or just use a symbol if
;; default should be nil.

;;; Results to be computed are defined via defres.  Arguments are an
;;; id (any lisp form) and a body of expressions to be evaluated to
;;; yield the value.
;;;
;;; Note that the transformation pipeline can give meaning to
;;; otherwise invalid expressions, making it possible to define DSLs
;;; for use with makeres which would be unwieldy otherwise.

;; Notice that (par source) is used to refer to the parameter "source"
(defres filtered
  (print 'filtered)
  (remove-if (lambda (x)
               (< x 5))
             (par source)))

;; Notice that (res filtered) is used to refer to the result target
;; "filtered"
(defres squared
  (print 'squared)
  (mapcar (lambda (x)
            (* x x))
          (res filtered)))

;; And this combines par and res.  Also notice that target ids can be
;; any form, not just symbols
(defres (sum scaled)
  (print '(sum scaled))
  (* (par scale)
     (+ (res filtered)
        (res squared))))

;;; execute (makeres) to test.  makeres accepts keyword arguments for
;;; whatever have been defined via defpars.
(makeres)
;; or with other arguments:
(makeres :source (list 9 10))
(makeres :source (list 9 15)
         :scale -1)

;;; Technically, makeres is an inefficient technique if one wishes to
;;; run the computation for various parameter values; one should use
;;; compres.

;; how you use compres:
(defparameter *generator*
  (compres)
  "Computation function which accepts any parameters for this project
and computes only those values which need computing either due to not
being previously computed, having their status set to nil, or
depending on a parameter value which was not used last.")

;; default args
(funcall *generator*)
(funcall *generator*
         :scale -1)
(funcall *generator*
         :source (list 11 12)
         :scale 5)

;;; After you've run whatever computations you're interested in, you
;;; can examine the results with the res macro:

(print (res filtered))
(print (res squared))
(print (res (sum scaled)))

;;; You can also examine what the last parameter values were:

(print (par source))
(print (par scale))

;;;; ADDING GRAPH TRANSFORMATIONS

;;; Graph transformations are the key ingredient to making makeres
;;; useful in practice.
;;;
;;; We can define new declarative or imperative operators, however you
;;; wish to interpret them via your own transformations.
;;;
;;; For a quick example, let's create a new project to test out
;;; transformations:

(in-project transform)

;;; Let's define a new operator, table-pass, which is used to denote
;;; passing over a table (cl-ana table).  We'll define the operator as
;;;
;;; (table-pass table fields inits return &body body)
;;;
;;; which places each binding in inits, executes body once per row
;;; using field selections from fields, and finally returning the
;;; return form outside the loop but inside the let expression.
;;;
;;; The rules:
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
;;; A suitable approach is to remove all table-pass results and
;;; replace them with parallelized targets which return lists of all
;;; needed results.  The final targets can be added as dependencies of
;;; these returned lists.

;;; Let's define the transformation:

;; for ease of editor use, define a macro:
(defmacro table-pass (table fields inits result &body body)
  nil)

;; must be available at compile and load times:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun table-trans (target-table)
    "Transforms target-table according to table-pass operator."
    (let* (;; table-pass ids:
           (tp-ids
            ;; Must handle progn in table-expr, always present for
            ;; ease of use.
            (loop
               for id being the hash-keys in target-table
               for tar being the hash-values in target-table
               when (and (second (target-expr tar))
                         (listp (second (target-expr tar)))
                         (eq (first (second (target-expr tar)))
                             'table-pass))
               collect id)))
      ;;;; Current idea for algorithm: Group table pass target ids
      ;;;; according to table (could use a map from table form to
      ;;;; table pass target ids) and pass number.  Still need
      ;;;; algorithm to determine pass number.  Current table-pass
      ;;;; semantics only allows single table passing, so hash-table
      ;;;; from table form to target id would work.
      (print tp-ids)
      target-table)))

(settrans (table-trans))

;; source table
(defres table
  (wrap-for-reuse
   (open-plist-table '((:field 1)
                       (:field 2)
                       (:field 1.5)
                       (:field 3)))))

;; average:
(defres mean
  (table-pass (res table) ("field")
      ((sum 0)
       (count 0))
      (/ sum count)
    (incf sum field)
    (incf count)))

(defres sigma
  (table-pass (res table) ("field")
      ((sum-squares 0)
       (count 0)) ; safe since sigma happens in second pass
      (sqrt (/ sum-squares
               (1- count)))
    (incf sum-squares
          (expt (- field
                   (res mean))
                2))
    (incf count)))
