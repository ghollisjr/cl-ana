;;;; macro-utils.lisp

(in-package :macro-utils)

;; (defmacro with-gensyms (vars &body body)

;;   "Masks the variables listed in vars with (gensym) symbol names.
;; When writing a body expression using with-gensyms, be sure to place
;; the comma in front of variables referring to the (gensym) variables,
;; as otherwise it will refer a different binding (which sometimes is
;; exactly what you want)."

;;   (let ((bindings (mapcar #'(lambda (v) `(,v (gensym))) vars)))
;;     `(let ,bindings
;;        ,@body)))

(defmacro inrange (xlo op1 x op2 xhi &key (prec 0))
  `(and (if ,xlo
	    (,op1 (- ,xlo ,prec) ,x)
	    t)
	(if ,xhi
	    (,op2 ,x (+ ,xhi ,prec))
	    t)))

(defmacro print-eval (arg)
  (with-gensyms (varname)
    `(let ((,varname ,arg))
       (format t "~a: ~a~%" ',arg ,varname)
       ,varname)))
