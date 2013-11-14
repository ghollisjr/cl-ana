;;;; package.lisp

(defpackage #:generic-math
  (:nicknames :gmath)
  (:use :cl)
  (:shadow :+
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
