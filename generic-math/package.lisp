;;;; package.lisp

(defpackage #:generic-math
  (:nicknames :gmath)
  (:use :cl
	:list-utils)
  (:shadow :incf
	   :decf
	   :+
	   :-
	   :*
	   :/
	   :sqrt
	   :expt
	   :exp
	   :log
	   :sin
	   :cos
	   :tan
	   :sinh
	   :cosh
	   :tanh)
  (:export :use-gmath
	   :defmethod-commutative
	   :+
	   :sum
	   :add
	   :-
	   :sub
	   :unary-sub
	   :*
	   :mult
	   :/
	   :protected-/
	   :div
	   :unary-div
	   :protected-div
	   :protected-unary-div
	   :sqrt
	   :expt
	   :exp
	   :log
	   :sin
	   :cos
	   :tan
	   :sec
	   :csc
	   :cot
	   :sinh
	   :cosh
	   :tanh
	   :sech
	   :csch
	   :coth))
