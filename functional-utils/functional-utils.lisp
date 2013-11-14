;;;; functional-utils.lisp

(in-package :functional-utils)

(defun flip (f)
  "Takes a function of two parameters and creates a new function which
  takes the same parameters in reverse order; particularly useful in
  conjunction with reduce."
  #'(lambda (x y) (funcall f y x)))

;; (defun compose (&rest fs)
;;   "Arbitrary number of functions composed"
;;   (let ((f (first fs)))
;;     (if (= 1 (length fs))
;; 	(first fs)
;; 	#'(lambda (&rest xs)
;; 	    (funcall f (apply (apply #'compose (rest fs)) xs))))))

(defun to-pair-function (f)
  "Makes f apply to a pair instead of two arguments"
  #'(lambda (x) (funcall f (car x ) (cdr x))))
