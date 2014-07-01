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
