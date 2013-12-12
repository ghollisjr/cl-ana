;;;; number.lisp

;;;; Implements generic math functions for numbers

(in-package :generic-math)

;;; I know I could create a macro to automate this redundant code,
;;; maybe I will at some point, but I don't want to obfuscate the code
;;; too much for the poor readers.

(declaim (optimize (speed 3)
                   (safety 0)
                   (compilation-speed 0)
                   (debug 0)))

(defmethod add ((x number) (y number))
  (cl:+ x y))

(defmethod sub ((x number) (y number))
  (cl:- x y))

(defmethod unary-sub ((x number))
  (cl:- x))

(defmethod mult ((x number) (y number))
  (cl:* x y))

(defmethod div ((x number) (y number))
  (cl:/ x y))

(defmethod unary-div ((x number))
  (cl:/ x))

(defmethod protected-div (x (y number)
			  &key
			    (protected-value 0))
  (if (zerop y)
      protected-value
      (div x y)))

(defmethod protected-unary-div ((x number)
				&key
				  (protected-value 0))
  (if (zerop x)
      protected-value
      (unary-div x)))

(defmethod sqrt ((x number))
  (cl:sqrt x))

(defmethod expt ((x number) (y number))
  (cl:expt x y))

(defmethod exp ((x number))
  (cl:exp x))

(defmethod log ((x number))
  (cl:log x))

(defmethod sin ((x number))
  (cl:sin x))

(defmethod cos ((x number))
  (cl:cos x))

(defmethod tan ((x number))
  (cl:tan x))

;; methods for sec, csc, cot are handled well by defaults

(defmethod sinh ((x number))
  (cl:sinh x))

(defmethod cosh ((x number))
  (cl:cosh x))

(defmethod tanh ((x number))
  (cl:tanh x))

;; methods for sech, csch, tanh are handled well by defaults

;;; can add more as I think of them/look up what should be included
