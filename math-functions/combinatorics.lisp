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

(in-package :cl-ana.math-functions)

(defmath factorial (x)
  (:documentation "Very naive factorial function")
  (:method ((x number))
    (labels ((rec (x &optional (result 1))
               (if (<= x 1)
                   result
                   (rec (1- x) (* x result)))))
      (rec x))))

(defmath npermutations (n r)
  (:documentation "Returns nPr, the number of permutations of n objects
taken r at a time without repetition.  Assumes reasonable values of n
and r.  Returns 1 for nP0.")
  (:method ((n number) (r number))
    (loop
       for k from (- n r) upto n
       for result = 1 then (* result k)
       finally (return result))))

(defmath npermutations-repeating (n r)
  (:documentation "Returns n^r, the number of ways to permute r
  objects from a set of n with allowed repetition.  Assumes reasonable
  values of n and r.  Note that r can be greater than n.")
  (:method ((n number) (r number))
    (expt n r)))

(defmath ncombinations (n r)
  (:documentation "Returns nCr, the number of combinations of n
  objects taken r at a time without repetition.  Assumes reasonable
  values of n and r.")
  (:method ((n number) (r number))
    (cond
      ((zerop r)
       1)
      ((= n r)
       1)
      (t
       (let ((result 1)
             (r (min r (- n r)))) ; symmetry optimization
         (do ((num n (1- num))
              (den 1 (1+ den)))
             ((> den r) result)
           (setf result (* result num))
           (setf result (truncate result den))))))))

(defun ncombinations-repeating (n r)
  "Returns the number of combinations of n objects taken r at a time
  with allowed repetition.  Assumes reasonable values of n and r.
  Note that r can be greater than n."
  (ncombinations (1- (+ n r)) r))

(defun binomial (n r)
  "Nickname for ncombinations; returns the binomial coefficient of n
  and r."
  (ncombinations n r))

(defun multinomial (&rest ms)
  "Returns the multinomial coefficient where each element of ms is
taken to be the number of objects of a single type."
  (/ (factorial (sum ms))
     (product (mapcar #'factorial ms))))

;;;; Useful macros for working with combinatorics
;;; Loop over all nPr permutations.  E.g.,
;;;
;;; (for-permutations (indices 5 2) (print indices))
;;;
;;; would print out all of the possible 20 permutation index arrays.
;;;
;;; Note that for nP0, a single iteration of body is evaluated with an
;;; empty index array.
(defmacro for-permutations ((indexvar n r) &body body)
  "Iterates over all permutations of r objects taken from n total,
binding an array of r index values to indexvar and evaluating body
with that binding.  If you want to actually permute objects in a
list/array/sequence, use the bound index array to find permutations of
those objects.  No safety checks are performed, so use reasonable
values of n and r."
  (alexandria:with-gensyms (npr nn rr i j norder x k)
    `(let* ((,nn ,n)
            (,rr ,r))
       ;; safety check
       (when (and (plusp ,nn)
                  (<= 0 ,rr ,nn))
         (let* ((,npr (npermutations ,nn ,rr))
                (,indexvar (make-array ,rr :element-type 'integer :initial-element 0)))
           (loop
              for ,i below ,npr
              do
                (loop
                   for ,j below ,rr
                   do
                     (let* ((,norder (- ,nn ,j))
                            (,x (mod ,i ,norder)))
                       (setf (aref ,indexvar ,j)
                             ,x)
                       (loop
                          for ,k below ,j
                          when (>= ,x
                                   (aref ,indexvar ,k))
                          do (incf (aref ,indexvar ,j)))))
                (progn
                  ,@body)))))))

;; Same but with allowed repetition
(defmacro for-permutations-repeating ((indexvar n r) &body body)
  "Iterates over all permutations with allowed repitition of r objects
 taken from n total, binding an array of r index values to indexvar
 and evaluating body with that binding.  If you want to actually
 permute objects in a list/array/sequence, use the bound index array
 to find permutations of those objects.  No safety checks are
 performed, so use reasonable values of n and r.  Note that r can be
 greater than n since repetition is allowed."
  (alexandria:with-gensyms (npr nn rr i j)
    `(let* ((,nn ,n)
            (,rr ,r))
       ;; safety check
       (when (and (plusp ,nn)
                  (>= ,rr 0))
         (let* ((,npr (npermutations-repeating ,nn ,rr))
                (,indexvar (make-array ,rr :element-type 'integer :initial-element 0)))

           (loop
              for ,i below ,npr
              do
                (loop
                   for ,j below ,rr
                   do
                     (setf (aref ,indexvar ,j)
                           (mod (truncate ,i (expt ,nn ,j))
                                ,nn)))
                (progn
                  ,@body)))))))

;;; Loop over all nCr combinations.  E.g.,
;;;
;;; (for-combinations (indices 5 2) (print indices))
;;;
;;; would print out all of the possible 10 combination index arrays.
;;;
;;; Note that for nC0, a single iteration of body is evaluated with an
;;; empty index array.
(defmacro for-combinations ((indexvar n r) &body body)
  "Evaluates body for every combination of r objects taken from a set
of n without repetition and without caring about order of occurrence.
indexvar will be set to an index array denoting the selected
combination.  For unreasonable values of n and r, no iterations are
performed.  Note that for r=0, one evaluation of the body is performed
with an empty index array."
  (alexandria:with-gensyms (n-r
                            nn rr i
                            lastindex currentindex state
                            current nextindex next
                            delta k)
    `(let* ((,nn ,n)
            (,rr ,r)
            (,n-r (- ,nn ,rr))) ; useful for later
       ;; safety check
       (when (and (plusp ,nn)
                  (<= 0 ,rr ,nn))
         (let* ((,indexvar
                 (make-array ,rr :element-type 'integer :initial-element 0)))

           ;; init indices
           (loop
              for ,i below ,rr
              do (setf (aref ,indexvar ,i) ,i))
           ;; strategy:
           ;;
           ;; Indices will be strictly ordered from least to greatest.
           ;; This makes finding the next valid combination of indices
           ;; simple.
           ;;
           ;; Iteration is accomplished through a state machine with the
           ;; following states:
           ;;
           ;; 0. Execute combination.
           ;; 1. Increment.
           ;; 2. Halt.
           ;;
           ;; There is an additional state variable: currentindex is the
           ;; index currently attempted to increment.  If it's not the
           ;; last index, then successfully finding a valid slot means
           ;; needing to initialize all higher indices.
           (let* ((,lastindex (1- ,rr))
                  (,currentindex ,lastindex)
                  (,state 0))
             (symbol-macrolet ((,current
                                (aref ,indexvar ,currentindex))
                               (,nextindex (1+ ,currentindex))
                               (,next (aref ,indexvar (1+ ,currentindex))))
               (loop
                  while (not (= ,state 2))
                  do
                    (case ,state
                      ;; execute
                      (0
                       ,@body
                       (setf ,currentindex ,lastindex)
                       (setf ,state 1))
                      ;; increment
                      (1
                       (cond
                         ;; out of combinations
                         ((< ,currentindex 0)
                          (setf ,state 2))
                         ;; out of room to grow for increasing currentindex
                         ((< ,n-r
                             (- (1+ ,current) ,currentindex))
                          (decf ,currentindex))
                         ;; can find combination
                         (t
                          (incf ,current)
                          (let* ((,delta (- ,current ,currentindex)))
                            (loop
                               for ,k from (1+ ,currentindex) below ,rr
                               do (setf (aref ,indexvar ,k)
                                        (+ ,k ,delta))))
                          (setf ,state 0)
                          (setf ,currentindex ,lastindex)))))))))))))

(defmacro for-combinations-repeating ((indexvar n r) &body body)
  "Evaluates body for every combination of r objects taken from a set
of n with allowed repetition but without caring about order of
occurrence.  indexvar will be set to an index array denoting the
selected combination.  For unreasonable values of n and r, no
iterations are performed.  Note that for r=0, one evaluation of the
body is performed with an empty index array."
  (alexandria:with-gensyms (n-1
                            nn rr i
                            lastindex currentindex state
                            current nextindex next
                            k)
    `(let* ((,nn ,n)
            (,rr ,r)
            (,n-1 (1- ,nn))) ; useful for later
       ;; safety check
       (when (and (plusp ,nn)
                  (>= ,rr 0))
         (let* ((,indexvar
                 (make-array ,rr :element-type 'integer :initial-element 0)))
           ;; init indices
           (loop
              for ,i below ,rr
              do (setf (aref ,indexvar ,i) 0))
           ;; strategy:
           ;;
           ;; Indices will be strictly ordered from least to greatest.
           ;; This makes finding the next valid combination of indices
           ;; simple.
           ;;
           ;; Iteration is accomplished through a state machine with the
           ;; following states:
           ;;
           ;; 0. Execute combination.
           ;; 1. Increment.
           ;; 2. Halt.
           ;;
           ;; There is an additional state variable: currentindex is the
           ;; index currently attempted to increment.  If it's not the
           ;; last index, then successfully finding a valid slot means
           ;; needing to initialize all higher indices.
           (let* ((,lastindex (1- ,rr))
                  (,currentindex ,lastindex)
                  (,state 0))
             (symbol-macrolet ((,current
                                (aref ,indexvar ,currentindex))
                               (,nextindex (1+ ,currentindex))
                               (,next (aref ,indexvar (1+ ,currentindex))))
               (loop
                  while (not (= ,state 2))
                  do
                    (case ,state
                      ;; execute
                      (0
                       ,@body
                       (setf ,currentindex ,lastindex)
                       (setf ,state 1))
                      ;; increment
                      (1
                       (cond
                         ;; out of combinations
                         ((< ,currentindex 0)
                          (setf ,state 2))
                         ;; out of room to grow for increasing currentindex
                         ((<= ,n-1
                              ,current)
                          (decf ,currentindex))
                         ;; can find combination
                         (t
                          (incf ,current)
                          (loop
                             for ,k from (1+ ,currentindex) below ,rr
                             do (setf (aref ,indexvar ,k)
                                      ,current))
                          (setf ,state 0)
                          (setf ,currentindex ,lastindex)))))))))))))
