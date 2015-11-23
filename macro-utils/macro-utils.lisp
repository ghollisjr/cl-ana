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

(in-package :cl-ana.macro-utils)

;; Macro for defining pluralized versions of functions, e.g. a
;; function which would map car across a list could be called cars.
(defmacro defplural (fname)
  "Defines a function which maps the function with symbol fname across
a list.  The defined function has as its symbol fname with a trailing
s character appended."
  (let* ((fname-str (string fname))
         (fnames-str (string-append fname-str "S"))
         (fnames (intern fnames-str)))
    `(defun ,fnames (list)
       (mapcar #',fname list))))

;; Macro for evaluating multiple functions on a single object,
;; creating bindings for the return values:
(defmacro function-value-bind (function-symbols object &body body)
  "Evaluates each function in the list of function symbols on object
in the order as they occur in function-symbols.  Binds each return
value to the corresponding function-symbol and executes body inside of
this lexical scope."
  (with-gensyms (obj)
    `(let* ((,obj ,object)
            ,@(loop
                 for fs in function-symbols
                 collect `(,fs (,fs ,obj))))
       ,@body)))

(defmacro map-bind (fn symbols &body body)
  "Binds each symbol in symbols to the value of (funcall fn symbol) and
  executes body inside of this lexical scope."
  (with-gensyms (func)
    `(let* ((,func ,fn)
            ,@(loop
                 for sym in symbols
                 collecting `(,sym (funcall ,func ,sym))))
       ,@body)))

;; A little help from On Lisp
(defmacro abbrev (short long)
  "Defines abbreviated operator with name short expanding to a call to
long."
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  "Defines multiple abbreviations simultaneously.  Arguments are interpreted as:

(abbrevs short1 long1
         short2 long2
         ...)"
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

(abbrevs dbind destructuring-bind
         mvbind multiple-value-bind
         mvsetq multiple-value-setq
         fvbind function-value-bind
         mbind map-bind)

;; Polling macro: Polling is fairly common in asynchronous programs,
;; and due to this having a premade macro comes in handy
(defmacro poll (test wait &body body)
  "Repeatedly executes test form; if test returns true, executes body;
otherwise waits for wait seconds and tries again.  Returns last form
in body."
  (alexandria:with-gensyms (tst)
    `(do ()
         (,test (progn ,@body))
       (sleep ,wait))))

(defmacro inrange (xlo op1 x op2 xhi &key (prec 0))
  `(and (if ,xlo
	    (,op1 (- ,xlo ,prec) ,x)
	    t)
	(if ,xhi
	    (,op2 ,x (+ ,xhi ,prec))
	    t)))

;; Macro for limited case using equal for comparison:
(defmacro case-equal (form &body cases)
  (with-gensyms (ev-form)
    (let* ((otherwise nil)
           (tests
            (loop for c in cases
               when (or (eq (car c) 'otherwise)
                        (eq (car c) 't))
               do (setf otherwise (cdr c))
               else
               collecting `(equal ,ev-form ,(car c))))
           (bodies (mapcar #'cdr cases)))
      `(let ((,ev-form ,form))
         (cond
           ,@(nconc (zip tests bodies)
                    (when otherwise
                      `((t ,@otherwise)))))))))

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
       (prin1 ',arg)
       (format t ": ")
       (prin1 ,varname)
       (format t "~%")
       ,varname)))

;; Calls a function with default argument values
(defmacro with-default-args (fn (&rest argbindings) &body body)
  "Executes body with default argument values supplied to fn in a convenient way.

fn is either the name of a function or a list (function local-name);
fn is defined via flet and takes no arguments, but refers to the
arguments in argbindings.

argbindings are used analogously to let.

Example: (with-default-args (list fn) ((x 3)) (fn)) yields (3).  More
useful examples would involve conditionally setting argument values in
the body."
  `(let (,@argbindings)
     (flet ((,(if (listp fn)
                  (second fn)
                  fn)
                ()
              (funcall (symbol-function ',(if (listp fn)
                                              (first fn)
                                              fn))
                       ,@(mapcar #'car argbindings))))
       ,@body)))

;; Very useful macro for combining keyword arguments only when
;; appropriate
(defmacro when-keywords (&body keyword-arg-specs)
  "Creates a plist containing the keyword arguments only when the
values are non-nil; if a keyword-arg-spec is a list, then the first
element is taken to be the field symbol and the second element the
expression to be passed as the value."
  (let* ((specs
          (loop
             for kas in keyword-arg-specs
             collecting
               (if (consp kas)
                   kas
                   (list (keywordify kas)
                         kas))))
         (when-statements
          (loop
             for (field-name val) in specs
             collecting
               `(when ,val
                  (list ,field-name ,val)))))
    `(append ,@when-statements)))

;; Reader macro for when-keywords:
;; Use #k(fn arg1 ... &when-keys key1 ...) as a shorthand form for
;; (apply #'fn arg 1 ... (when-keywords key1 ...))
;;(eval-when (:compile-toplevel :load-toplevel :execute)
(defun when-keywords-transformer-reader-macro (stream subchar arg)
  (let ((expr (read stream t)))
    (multiple-value-bind (normal-terms when-keys)
        (loop
           for x in expr
           for lst on expr
           until (and (symbolp x) (equal (keywordify x) :&when-keys))
           collecting x into normal-terms
           finally (return (values normal-terms (rest lst))))
      (let ((fn (first normal-terms))
            (normal-args (rest normal-terms)))
        (append `(apply (function ,fn))
                normal-args
                (when when-keys
                  `((when-keywords ,@when-keys))))))))


(set-dispatch-macro-character
 #\# #\k #'when-keywords-transformer-reader-macro)
;;)

;; Sometimes this is useful:
(defmacro defun-with-setf (fname (&rest lambda-list) &body body)
  "Defines function along with setfable version when body is a single
expression; throws error otherwise.  This is limited to cases where
the expression is already understandable to setf."
  (if (length-equal body 1)
      (alexandria:with-gensyms (value)
        `(progn
           (defun ,fname ,lambda-list
             ,@body)
           (defun (setf ,fname) (,value ,@lambda-list)
             (setf ,@body
                   ,value))))
      (error "Cannot define-with-setf for more than one body expression")))

;;;; From let-over-lambda: (this should probably be somewhere else)
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;;;; Anaphoric macros from On Lisp:
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))

;; A more convenient version of the time function:

(defmacro time-proc (&body body)
  "Times the execution of body and returns multiple values: 1. Real
time in seconds, 2. Run time in seconds, and the rest being the return
values of body."
  (with-gensyms (ups
                 real-start-time
                 real-end-time
                 run-start-time
                 run-end-time
                 values)
    `(let ((,ups internal-time-units-per-second)
           (,real-start-time
            (get-internal-real-time))
           (,run-start-time (get-internal-run-time))
           (,values
            (multiple-value-list
             (progn ,@body)))
           (,run-end-time (get-internal-run-time))
           (,real-end-time (get-internal-real-time)))
       (apply #'values
              (/ (float (- ,real-end-time ,real-start-time) 0d0)
                 (float ,ups 0d0))
              (/ (float (- ,run-end-time ,run-start-time) 0d0)
                 (float ,ups 0d0))
              ,values))))

;;;; This is actually fully implemented by both loop and the iterate
;;;; library
;;;; Useful for simple while loops:
;; (defmacro while (boolean &body body)
;;   "Executes boolean at the start of each iteration; if boolean is
;; non-nil then body is executed, else while exits.  At the moment the
;; return value is not specified.n"
;;   (with-gensyms (test)
;;     `(do ((,test ,boolean ,boolean))
;;          ((not ,test))
;;        ,@body)))

;;;; lambda-list functions:

(defun ll-type (lambda-list)
  "Returns the type of lambda-list for a function which lambda-list
corresponds to."
  (cond
    ((member '&key lambda-list)
     :key)
    ((member '&rest lambda-list)
     :rest)
    ((member '&optional lambda-list)
     :optional)
    (t t)))

(defun args->keyword-args (args)
  (mapcan (lambda (x)
            (macrolet ((key (x)
                         `(keywordify (lispify ,x))))
              (if (listp x)
                  (destructuring-bind (arg arg-def)
                      x
                    (list (key arg)
                          arg))
                  (list (key x)
                        x))))
          args))

(defun lambda-list-call-form (fname lambda-list)
  "Returns the appropriate lambda-list call form for a function named
fname and a lambda-list given by lambda-list.  Macros are more
difficult since lambda lists can have nested structure, so I'm only
considering function lambda lists for now.  This still works for
macros which have this limited sort of lambda list however.

One issue that is not currently resolved is supplying arguments which
have a -supplied-p argument.  Functions can be handled, but not in the
same way as macros (since apply does not work).

Also: &rest is handled only for functions becuause, again, there is no
practical way to use this method for macros."
  (case (ll-type lambda-list)
    (:key
     (destructuring-bind (xs key-xs)
         (split-sequence '&key lambda-list)
       `(,fname ,@xs ,@(args->keyword-args key-xs))))
    (:optional
     (destructuring-bind (xs optional-xs)
         (split-sequence '&optional lambda-list)
       `(,fname ,@xs ,@(mapcar (compose #'car #'mklist) optional-xs))))
    (:rest
     (destructuring-bind (xs rest-xs)
         (split-sequence '&rest lambda-list)
       `(apply #',fname ,@xs ,(car rest-xs))))
    (t
     `(,fname ,@lambda-list))))

;; Macro for suppressing output:
(defmacro suppress-output (&body body)
  "suppress-output redirects all output to /dev/null, thus silencing
any messages printed to *standard-output*."
  `(with-open-file (*standard-output* "/dev/null"
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :error)
     (let ((*error-output* *standard-output*))
       ,@body)))

;; Modified lambda for keyword args
(defmacro klambda ((&rest key-args) &body body)
  "Modified lambda which returns a keyword arg-accepting function with
&allow-other-keys"
  `(lambda (&key ,@key-args &allow-other-keys)
     ,@body))

;; Only-when-present let macro
(defmacro olet ((&rest bindings) &body body)
  "olet (at the moment) functions much like let*, except that each
binding is only evaluated once at most, and not at all if the lexical
binding is never used in the body.  This can result in a tremendous
speedup when used in creating context, e.g. looping over a table but
only needing a few fields from the table.  In test cases the compiler
appeared to remove unused bindings entirely thanks to the
symbol-macrolet."
  (let* ((gsyms (mapcar (lambda (x) (gensym)) bindings))
         (gsets (mapcar (lambda (x) (gensym)) bindings))
         (sym-macros
          (mapcar (lambda (b gsym gset)
                    (destructuring-bind (sym val) b
                      `(,sym
                        (if ,gset
                            ,gsym
                            (progn
                              (setf ,gset t)
                              (setf ,gsym ,val))))))
                  bindings
                  gsyms
                  gsets)))
    `(let (,@gsyms
           ,@gsets)
       (symbol-macrolet ,sym-macros
         ,@body))))

;; dlambda from let over lambda
(defmacro dlambda (&rest ds)
  (alexandria:with-gensyms (args)
    `(lambda (&rest ,args)
       (case (car ,args)
         ,@(mapcar
            (lambda (d)
              `(,(if (eq t (car d))
                     t
                     (list (car d)))
                 (apply (lambda ,@(cdr d))
                        ,(if (eq t (car d))
                             args
                             `(cdr ,args)))))
            ds)))))
