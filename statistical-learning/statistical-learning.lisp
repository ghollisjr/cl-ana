;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2019 Gary Hollis
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

(in-package :cl-ana.statistical-learning)

;;;; Clustering
;; K-means clustering
(defun k-means-cluster (data init)
  "Clusters the data using the k-means algorithm.  init can be a
number specifying the number of clusters to partition the data into,
or a list of explicit initial guesses for the centroids.

Data should be a list of numbers or lists.

Returns:

* The clusters of data.
* The centroids.
* The number of iterations required."
  (let* ((niter 0)
         (data (mapcar #'->double-float data))
         (centroids
          (if (listp init)
              (copy-list init)
              (subseq data 0 (min (length data) init))))
         (k (length centroids))
         (changed-p t))
    ;; data length check
    (when (<= (length data)
              (length centroids))
      (return-from k-means-cluster
        (values (map 'list #'list centroids)
                centroids
                0)))
    (let* ((categories (make-vector (length data)
                                    :initial-element -1))
           (clusters (make-vector k :initial-element nil))
           (normfn (if (listp (first data))
                       (lambda (x c)
                         (sum (expt (- x (car c)) 2)))
                       (lambda (x c)
                         (expt (- x (car c)) 2)))))
      ;; iterative algorithm
      (loop
         while changed-p
         do
           (incf niter)
           (setf changed-p nil)
           (loop
              for x in data
              for i from 0
              do (let* ((nearest
                         (minimum (zip centroids
                                       (range 0 (1- k)))
                                  :key (lambda (c)
                                         (funcall normfn x c))))
                        (newcategory (cdr nearest)))
                   (when (not (= newcategory (elt categories i)))
                     (setf changed-p t)
                     (setf (elt categories i)
                           newcategory)
                     (setf (elt centroids newcategory)
                           (mean (loop
                                    for x in data
                                    for c across categories
                                    when (= c newcategory)
                                    collecting x)))))))
      ;; collect categorized data:
      (loop
         for x in data
         for c across categories
         do (push x (elt clusters c)))
      (values (map 'list #'reverse clusters)
              centroids
              niter))))

;; G-means clustering
(defun g-means-cluster (data
                        &key (critical 2.492))
  "Applies the G-means clustering algorithm based on the
Anderson-Darling normality test.  The algorithm is a variant of
k-means where the number k is determined through finding the minimum
number of clusters required to have Gaussian distributed data when
projected along a single dimension.  Critical selects the critical
value for the normality test.

Data should be a list of numbers or lists.

Returns:
* Clusters
* Centroids
* k"
  (labels (;; project data onto new axis
           (project (x vec)
             (if (atom x)
                 x
                 (/ (euclidean-dot x vec)
                    (euclidean-norm vec))))
           ;; test for gaussian normality of a centroid
           (g-test (cluster)
             ;; Get new centroids
             (multiple-value-bind (clusters centroids niter)
                 (k-means-cluster cluster 2)
               (declare (ignore clusters niter))
               (let* (;; Find axis
                      (vec (apply #'- centroids))
                      ;; Project data onto that axis
                      (projected (mapcar (lambda (x)
                                           (project x vec))
                                         cluster)))
                 ;; apply normality test
                 (anderson-darling-normality-test projected
                                                  :critical critical)))))
    (let* ((n (length data)))
      (loop
         for k from 1 to n
         do (multiple-value-bind (clusters centroid niter)
                (k-means-cluster data k)
              (declare (ignore niter))
              (let* ((g-tests
                      (loop
                         for cluster in clusters
                         collecting (g-test cluster))))
                ;; (print k)
                (when (every #'identity g-tests)
                  (return (values clusters centroid k)))))))))

;;;; Least squares
(defun linear-least-squares (data)
  "Fits a linear function against the data alist.  car can be a list,
but cdr should be an atom.  Fitting a vector Y value is mathematically
equivalent to simultaneous indepedent linear fits so no functionality
is lost."
  (when data
    (let* ((coefs
            (coerce
             (loop
                for cons in data
                collecting (coerce (cons 1d0 (mklist (car cons))) 'vector))
             'vector))
           (coefstransposed
            (matrix-transpose coefs))
           (yvector
            (cdrs data)))
      (lu-solve (matrix-mult coefstransposed
                             coefs
                             'list)
                (first (transpose
                        (matrix-mult coefstransposed
                                     (map 'vector
                                          (lambda (x)
                                            (apply #'vector x))
                                          (transpose (list yvector)))
                                     'list)))))))

(defun ridge-regression (data lambda)
  "Applies ridge regression fitting as a function of lambda."
  (let* ((coefs (map 'vector (lambda (x)
                               (coerce (cons 1d0 (mklist x)) 'vector))
                     (cars data)))
         (coefstransposed
          (matrix-transpose coefs))
         (ys (mapcar #'cdr data))
         (newcoefs (matrix-mult coefstransposed coefs 'list))
         (newys
          (first (transpose
                  (matrix-mult coefstransposed
                               (map 'vector (lambda (y)
                                              (apply #'vector y))
                                    (transpose (list ys)))
                               'list)))))
    (loop
       for i below (1- (length newcoefs))
       do (incf (tensor-ref newcoefs i i) lambda))
    (lu-solve newcoefs newys)))

(defun data->polynomial-x (data order)
  "Returns an alist of data suitable for least squares fitting against
a polynomial function using the existing linear-least-squares fit
function.  order should be at least 0.  Note that this is not as
efficient as a purpose-built polynomial least squares fit due to
algebraic simplifications to the matrix multiplication that are
possible."
  (mapcar (lambda (cons)
            (destructuring-bind (x . y) cons
              (let* ((term 1d0))
                (cons (loop
                         for i from 1 upto order
                         do (setf term (* term x))
                         collecting term)
                      y))))
          data))

(defun fit-polynomial-least-squares (data degree)
  "Fits a polynomial of specified degree against the data, returning
the list of parameters from least order to highest order."
  (let* ((coefs
          (loop
             for k from 0 to degree
             collecting
               (loop
                  for j from 0 to degree
                  for pow = (+ k j)
                  collecting (loop
                                for (x . y) in data
                                summing (expt x pow)))))
         (constants
          (loop
             for k from 0 to degree
             collecting
               (loop
                  for (x . y) in data
                  summing (* y (expt x k))))))
    (lu-solve coefs constants)))

;;;; Kernel methods
(defun gaussian-kernel (x1 x2 lambda)
  "Gaussian kernel for multi-dimensional x1 and x2"
  (* (/ lambda)
     (exp
      (- (/ (if (sequencep x1)
                (sum (expt (- x1 x2) 2d0))
                (expt (- x1 x2) 2d0))
            lambda)))))
(defun make-gaussian-kernel (lambda)
  (lambda (x1 x2)
    (gaussian-kernel x1 x2 lambda)))

(defun kernel-method (data fn kernel init-params)
  "Returns a function which will compute the kernel method estimate
for a given point, using the init-params to fit on a point-by-point
basis."
  (lambda (x)
    (let* ((weights (mapcar (lambda (y)
                              (funcall kernel x y))
                            (cars data)))
           ;; X is now (x, y, sqrt(w)) and y is always 0
           (ndata (mapcar (lambda (cons w)
                            (cons (list (car cons)
                                        (cdr cons)
                                        (sqrt w))
                                  0d0))
                          data
                          weights))
           (nfn (lambda (params k)
                  (destructuring-bind (x y w) k
                    (* w
                       (- (funcall fn params x) y)))))
           (fit-results
            (suppress-output
             (multiple-value-list
              (fit ndata nfn init-params))))
           (fit-params (second fit-results)))
      (funcall fn fit-params x))))

(defun kernel-polynomial-method (data kernel degree)
  "Returns a function which will compute the kernel method estimate
for a given point, using a polynomial of the specified degree."
  (lambda (x)
    (let* ((weights (mapcar (lambda (y)
                              (funcall kernel x y))
                            (cars data)))
           (coefs
            (loop
               for k from 0 to degree
               collecting
                 (loop
                    for j from 0 to degree
                    for pow = (+ k j)
                    collecting (loop
                                  for (x . y) in data
                                  for w in weights
                                  summing (* w (expt x pow))))))
           (constants
            (loop
               for k from 0 to degree
               collecting
                 (loop
                    for (x . y) in data
                    for w in weights
                    summing (* y w (expt x k)))))
           (fit-params (lu-solve coefs constants)))
      (polynomial fit-params x))))
