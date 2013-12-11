(require 'typespec)

(in-package :typespec)

(defparameter *ts* '(:compound
                     ("x" . (:array :int 2 (3 3)))
                     ("y" . :string)))

(defparameter *struct* 
  '(x ((1 2 3) (4 5 6) (7 8 9))
    y "hello"))

(defparameter *x* (typespec-foreign-alloc *ts*))

(defparameter *lisp->c-converter* (typespec->lisp-to-c *ts*))

(funcall *lisp->c-converter* *struct* *x*)

(defparameter *c->lisp-converter*
  (typespec->c-to-lisp *ts*))

(print (funcall *c->lisp-converter* *x*))
