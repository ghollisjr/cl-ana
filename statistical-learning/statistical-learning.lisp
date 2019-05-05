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
