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
;;;; tensor.lisp

(in-package :tensor)

;;;; A tensor is just a recursively structured sequence of sequences
;;;; of ....  It's just the generalization of the idea of a flat
;;;; sequence, and is useful for implementing things like contiguous
;;;; histograms, matrices, actual tensors, etc.
;;;;
;;;; To make use of the tensor functions, just define a list, or
;;;; vector of vectors, or even a list of vectors of lists of ..., you
;;;; get the idea.
;;;;
;;;; Of particular merit is the fact that everything is vectorized
;;;; automatically when using this library; you want to add a constant
;;;; value to a list?  Just do (+ (list x y z ...) value).  Want to
;;;; take the square root of an array of numbers?  Just do (sqrt
;;;; array).

(declaim (optimize (speed 2)
                   (safety 1)
                   (compilation-speed 0)
                   (debug 1)))

;;; Reader macro: #t(fn tensor1 tensor2 ...) applies tensor-map to the
;;; arguments.  Can use #' or (function) on fn or not, both cases are
;;; handled.
(defun tensor-map-transformer-reader-macro (stream subchar arg)
  (let* ((expr (read stream t))
         (raw-fn (first expr))
         (fn (if (symbolp raw-fn)
                 `(function ,raw-fn)
                 raw-fn))
         (args (rest expr)))
    `(tensor-map ,fn ,@args)))

(set-dispatch-macro-character
 #\# #\t #'tensor-map-transformer-reader-macro)

(defun define-tensor-methods ()
  (loop
     for f being the hash-keys in *gmath-generic-map*
     for a being the hash-values in *gmath-generic-map*
     do (define-tensor-method f a)))

(defun define-tensor-method (fname args)
  (multiple-value-bind (tensor-args key-args)
      (let ((key-pos (position "&KEY" args
                               :test #'equal
                               :key #'string)))
        (if key-pos
            (values (subseq args 0 key-pos)
                    (subseq args (1+ key-pos)))
            (values args nil)))
    (let* ((key-fields (mapcar #'keywordify key-args))
           (key-applied-args
            (loop
               for a in key-args
               for k in key-fields
               append (list k a)))
           (tensor-specialized-args
            (mapcar (lambda (x)
                      `(,x sequence))
                    tensor-args))
           (arg-specs
            (remove-if-not
             (lambda (x) (some #'listp x))
             (apply #'cartesian-product
                    (mapcar #'list
                            tensor-args
                            tensor-specialized-args)))))
      (loop
         for a in arg-specs
         do
           (multiple-value-bind (unspecialized specialized)
               (loop for i in a
                  if (symbolp i)
                  collect i into unspecialized
                  else collect (first i) into specialized
                  finally (return (values unspecialized
                                          specialized)))
             ;; this seems to be a necessary use of eval
             (eval
              `(defmethod ,fname
                   (,@a
                    ,@(when key-args
                            (cons '&key key-args)))
                 (tensor-map
                  (lambda (,@specialized)
                    (apply (function ,fname) ,@tensor-args
                           (list ,@key-applied-args)))
                  ,@specialized))))))))

;; Define tensor methods; note that this should be done after all
;; generic math functions have been defined with defmath.
(define-tensor-methods)

(defun sequencep (x)
  (subtypep (type-of x) 'sequence))

(defun make-tensor (dimension-list &key
                                     (type 'vector)
                                     (initial-element 0d0))
  "Creates a tensor with each dimension in dimension-list denoting the
length of the sequence at that dimension with the type of the
sequences given by the optional type argument."
  (if (single dimension-list)
      (make-sequence type (first dimension-list) :initial-element initial-element)
      (map type
	   (lambda (&rest xs)
             (make-tensor (rest dimension-list) :type type :initial-element initial-element))
	   (make-sequence type (first dimension-list)))))

(defun tensor-ref (tensor &rest subscripts)
  (if (sequencep tensor)
      (reduce #'elt subscripts :initial-value tensor)
      tensor))

(defun tensor-flat-ref (tensor subscript)
  (if (sequencep tensor)
      (let* ((dim-list (tensor-dimensions tensor))
             (subscripts
              (let ((sub subscript)
                    (result nil))
                (do* ((rdim-lst (reverse dim-list) (rest rdim-lst))
                      (dim-size (first rdim-lst) (first rdim-lst)))
                     ((null rdim-lst) result)
                  (push (mod sub dim-size)
                        result)
                  (setf sub (floor sub dim-size))))))
        (reduce #'elt subscripts :initial-value tensor))
      tensor))

(defun (setf tensor-flat-ref) (value tensor subscript)
  (when (sequencep tensor)
    (let* ((dim-list (tensor-dimensions tensor))
           (subscripts
            (let ((sub subscript)
                  (result nil))
              (do* ((rdim-lst (reverse dim-list) (rest rdim-lst))
                    (dim-size (first rdim-lst) (first rdim-lst)))
                   ((null rdim-lst) result)
                (push (mod sub dim-size)
                      result)
                (setf sub (floor sub dim-size))))))
      (setf (apply #'tensor-ref tensor subscripts)
            value))))

(defun (setf tensor-ref) (value tensor &rest subscripts)
  (when (sequencep tensor)
    (let* ((last-sequence
            (apply #'tensor-ref tensor (butlast subscripts)))
           (last-subscript (lastcar subscripts)))
      (setf (elt last-sequence last-subscript) value))))

(defun tensor-rank (tensor)
  (labels ((tensor-rank-worker (tensor &optional (result 1))
	     (let ((first-elt (elt tensor 0)))
	       (if (sequencep first-elt)
		   (tensor-rank-worker first-elt (1+ result))
		   result))))
    (if (sequencep tensor)
	(tensor-rank-worker tensor)
	0)))

(defun tensor-dimensions (tensor)
  "Returns a list of the sizes of the tensor for each dimension."
  (labels ((tensor-dimensions-worker (tensor &optional result)
	     (if (sequencep tensor)
		 (tensor-dimensions-worker (elt tensor 0)
                                           (cons (length tensor) result))
		 (nreverse result))))
    (tensor-dimensions-worker tensor)))

(defun tensor-size (tensor)
  (reduce #'* (tensor-dimensions tensor)))

(defun sequence-length (x)
  "Returns the length of a sequence for sequences, else returns nil."
  (when (sequencep x)
    (length x)))

(defun map* (type fn &rest xs)
  "map* behaves exactly like map except that non-sequences are treated
as arbitrarily deep sequences with uniform value (that of the
object)."
  (let* ((min-length
          (reduce #'min
                  (loop
                     for x in xs
                     appending (mklist
                                (sequence-length x)))))
         (result (make-tensor (list min-length) :type type)))
    (if (zerop min-length)
        nil
        (progn
          (loop
             for i below min-length
             do (setf (tensor-ref result i)
                      (apply fn
                             (mapcar (lambda (x)
                                       (tensor-ref x i))
                                     xs))))
          result))))

(defun tensor-map (fn &rest xs)
  (let ((first-sequence
         (find-if #'sequencep xs)))
    (if first-sequence
        ;; handle sequences
        (apply #'map* (type-of first-sequence) (curry #'tensor-map fn) xs)
        (apply fn xs))))

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

;; I do apologize for this extremely ugly code, but it does in fact
;; work.  If you touch this code there is something wrong with you.
;;
;; At the moment I have not implemented single tensor contraction
;; (i.e. contraction of multiple indices from a single tensor).
(defun tensor-contract (type &rest tensor-index-lists)
  "Contracts each tensor along the dimension specified by the
specified index, resulting in a tensor of recursively rectangular
sequences of type type.

Each tensor-index-list is a list containing 1. The tensor to contract,
2. The index denoting the dimension to contract along for this tensor.

It is just the mathematical notion of tensor contraction.

Example: multiplying matrices:

A . B ==> (tensor-contract 'vector (list A 1) (list B 0))

In words, contract tensors A and B along the second index of A and
the first index of B."
  (let* ((tensors (mapcar #'first tensor-index-lists))
	 (tensor-ranks (mapcar #'tensor-rank tensors))
	 (starting-indices (make-offsets (mapcar #'1- tensor-ranks)))
	 (contracted-dims (mapcar #'second tensor-index-lists))
	 (ref-fns ; a ref-fn is a function taking the final index list
                                        ; and another index, returning the value of the
                                        ; tensor for that ref-fn along the contracted
                                        ; dimension at the second index using the
                                        ; appropriate indices from the final index list
	  (mapcar
	   (lambda (tensor starting-index tensor-rank dim)
             (let ((ending-index (add starting-index (1- tensor-rank))))
               (lambda (final-index-list contracting-index)
                 (let* ((applicable-indices (subseq final-index-list
                                                    starting-index
                                                    ending-index))
                        (left-indices (subseq applicable-indices 0 dim))
                        (right-indices
                         (when (< dim (- tensor-rank 1))
                           (subseq applicable-indices dim))))
                   (apply #'tensor-ref tensor
                          (append left-indices
                                  (list contracting-index)
                                  right-indices))))))
	   tensors starting-indices tensor-ranks contracted-dims))
	 (contracted-dimension-length
	  (reduce #'min
		  (mapcar (lambda (tensor dim)
                            (elt (tensor-dimensions tensor) dim))
			  tensors contracted-dims)))
	 (dimension-lists
	  (mapcar
	   (lambda (tensor tensor-rank dim)
             (let ((dimensions (tensor-dimensions tensor)))
               (append (subseq dimensions 0 dim)
                       (when (< dim (1- tensor-rank))
                         (subseq dimensions (1+ dim))))))
	   tensors tensor-ranks contracted-dims))
	 (result-dimension-list (apply #'append dimension-lists))
	 (final-indices (apply #'cartesian-product
			       (loop
				  for dim-length in result-dimension-list
				  collect (range 0 (1- dim-length)))))
	 (result (when final-indices
		   (make-tensor result-dimension-list :type type))))
    (if final-indices
	(loop
	   for final-index in final-indices
	   do
             (setf (apply #'tensor-ref result final-index)
                   (apply #'+
                          (loop
                             for contracted-index
                             below contracted-dimension-length
                             collect
                               (apply
                                #'*
                                (mapcar (lambda (ref-fn)
                                          (funcall ref-fn
                                                   final-index
                                                   contracted-index))
                                        ref-fns))))))
	(setf result
	      (apply #'+
		     (loop
			for contracted-index
                        below contracted-dimension-length
			collect
			  (apply #'*
				 (mapcar
                                  (lambda (ref-fn)
                                    (funcall ref-fn
                                             nil
                                             contracted-index))
                                  ref-fns))))))
    result))
