;;;; macro-utils.lisp

(in-package :macro-utils)

;; (defmacro with-gensyms (vars &body body)

;;   "Masks the variables listed in vars with (gensym) symbol names.
;; When writing a body expression using with-gensyms, be sure to place
;; the comma in front of variables referring to the (gensym) variables,
;; as otherwise it will refer a different binding (which sometimes is
;; exactly what you want)."

;;   (let ((bindings (mapcar (lambda (v) `(,v (gensym))) vars)))
;;     `(let ,bindings
;;        ,@body)))

(defmacro inrange (xlo op1 x op2 xhi &key (prec 0))
  `(and (if ,xlo
	    (,op1 (- ,xlo ,prec) ,x)
	    t)
	(if ,xhi
	    (,op2 ,x (+ ,xhi ,prec))
	    t)))

(defmacro cond-setf (place value &optional (condition t))
  "Only sets the place when the condition is met.

condition may be one of three values: :place, :value, or :both.

:place specifies that the place must evaluate to a non-nil value,

:value specifies that the value must evaluate to a non-nil value, and

:both specifies that both place and value must evaluate to non-nil
values."
  (let ((test
	 (case condition
	   (:place
	    place)
	   (:value
	    value)
	   (:both
	    `(and ,place ,value))
	   (:otherwise
	    t))))
    `(when ,test
       (setf ,place ,value))))

(defmacro print-eval (arg)
  (with-gensyms (varname)
    `(let ((,varname ,arg))
       (format t "~a: ~a~%" ',arg ,varname)
       ,varname)))

;;;; From let-over-lambda:
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
