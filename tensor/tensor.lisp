;;;; tensor.lisp

(in-package :tensor)

(defun tensor-map (fn &rest xs)
  "Like map, but works on an arbitrary-depth of nested sequences"
  (if (some #'null xs)
      (error "empty list given to tensor-map")
      (if (not (subtypep (type-of (first xs)) 'sequence))
	  (apply fn xs)
	  (apply #'map (type-of (first xs)) (curry #'tensor-map fn) xs))))

(defun tensor-+ (&rest xs)
  "Convenient nickname for mapping + over tensors."
  (apply #'tensor-map #'+ xs))

(defun tensor-- (&rest xs)
  "Convenient nickname for mapping - over tensors."
  (apply #'tensor-map #'- xs))

(defun tensor-* (&rest xs)
  "Convenient nickname for mapping * over tensors."
  (apply #'tensor-map #'* xs))

(defun tensor-/ (&rest xs)
  "Convenient nickname for mapping / over tensors."
  (apply #'tensor-map #'/ xs))

;; (defun tensor-+ (&rest xs)
;;   "Treats nested-sequences as tensors of arbitrary rank and computes
;; the sum.  A generalization of my vector-add function from list-utils."
;;   (if (some #'null xs)
;;       (error "empty list given to tensor-+")
;;       (if (not (subtypep (type-of (first xs)) 'sequence))
;; 	  (apply #'+ xs)
;; 	  (apply #'map (type-of (first xs)) #'tensor-+ xs))))

;; (defun tensor-- (&rest xs)
;;   "Treats nested-sequences as tensors of arbitrary rank and computes
;; the difference.  A generalization of my vector-sub function from
;; list-utils."
;;   (if (some #'null xs)
;;       (error "empty list given to tensor--")
;;       (if (not (subtypep (type-of (first xs)) 'sequence))
;; 	  (apply #'- xs)
;; 	  (apply #'map (type-of (first xs)) #'tensor-- xs))))
