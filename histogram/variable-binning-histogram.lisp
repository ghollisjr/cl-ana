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

(in-package :cl-ana.histogram)

;; Each bin low edge is mapped to the count of for that bin.  The low
;; edge is chosen so that
(defclass variable-binning-histogram (histogram)
  ((dim-specs
    :accessor variable-binning-histogram-dim-specs
    :initarg :dim-specs
    :initform nil
    :documentation "List of bin edges for each dimension")
   (content
    :accessor variable-binning-histogram-content
    :initarg :content
    :initform (make-hash-table :test #'equal)
    :documentation "Hash table mapping from low-edge list to count")
   ;; internal usage:
   (binary-trees
    :accessor variable-binning-histogram-binary-trees
    :initarg :binary-trees
    :initform nil
    :documentation "List of binary trees for each dimension used to
    lookup bin content")
   (maxes
    :accessor variable-binning-histogram-maxes
    :initarg :maxes
    :initform nil
    :documentation "List of maximum values for each dimension; for
    optimization.")))

(defun make-variable-binning-histogram
    (names-specs
     &key
       (empty-bin-value 0)
       (default-increment 1))
  "Creates a variable-binning-histogram.  names-specs should be an
alist mapping from dimension name to the list of bin edges for that
dimension (i.e. a list of lists with the first elements being the
dimension names)."
  (make-instance 'variable-binning-histogram
                 :ndims (length names-specs)
                 :dim-names (mapcar #'car names-specs)
                 :empty-bin-value empty-bin-value
                 :default-increment default-increment
                 :dim-specs (mapcar #'cdr names-specs)
                 :binary-trees (mapcar #'make-balanced-tree
                                       (mapcar #'cdr names-specs))
                 :maxes (mapcar (lambda (x)
                                  (first (last x)))
                                names-specs)))

(defun make-vhist (names-specs
                   &key
                     (empty-bin-value 0)
                     (default-increment 1))
  (make-variable-binning-histogram
   names-specs
   :empty-bin-value empty-bin-value
   :default-increment default-increment))

(defmethod hist-insert ((hist variable-binning-histogram)
                        datum
                        &optional
                          weight)
  (with-accessors ((low-edges variable-binning-histogram-dim-specs)
                   (btrees variable-binning-histogram-binary-trees)
                   (content variable-binning-histogram-content)
                   (maxes variable-binning-histogram-maxes)
                   (empty-bin-value hist-empty-bin-value)
                   (default-increment hist-default-increment))
      hist
    (flet ((uncar (x)
             (if (consp x)
                 (car x)
                 x)))
      (let ((indices
             (mapcar (lambda (bt d h)
                       (when (< d h)
                         (uncar (bref bt d))))
                     btrees datum maxes)))
        (when (every #'identity indices)
          (let ((weight (if weight
                            weight
                            default-increment))
                (start (if (gethash indices content)
                           (gethash indices content)
                           empty-bin-value)))
            (setf (gethash indices content)
                  (+ start weight)))))))
  hist)

;; Map->alist
(defmethod map->alist ((obj variable-binning-histogram))
  (let* ((edges-list
          (variable-binning-histogram-dim-specs
           obj))
         (bin-widths-list
          (mapcar (lambda (edges)
                    (- (rest edges) edges))
                  edges-list))
         (edge->width-ht-list
          (loop
             for edges in edges-list
             for bin-widths in bin-widths-list
             collecting (let ((result (make-hash-table :test 'equal)))
                          (loop
                             for edge in edges
                             for width in bin-widths
                             do (setf (gethash edge result)
                                      width))
                          result)))
         (content
          (variable-binning-histogram-content obj)))
    (loop
       for point being the hash-keys in content
       for count being the hash-values in content
       collecting (cons (loop
                           for p in point
                           for edge->width in edge->width-ht-list
                           collecting (+ p
                                         (* 0.5
                                            (gethash p edge->width))))
                        count))))

;; Copying
(defmethod copy-hist ((obj variable-binning-histogram)
                      &optional empty-p)
  (let ((result (make-variable-binning-histogram
                 (zip (hist-dim-names obj)
                      (variable-binning-histogram-dim-specs
                       obj))
                 :empty-bin-value
                 (hist-empty-bin-value obj)
                 :default-increment
                 (hist-default-increment obj))))
    (if empty-p
        result
        (let ((ht (variable-binning-histogram-content obj))
              (result-ht
               (variable-binning-histogram-content result)))
          (loop
             for indices being the hash-keys in ht
             for count being the hash-values in ht
             do (setf (gethash indices result-ht)
                      count))
          result))))

;; Arithmetic functions:

;; Assumes that the histograms have congruent binnings
(defmethod add ((h1 variable-binning-histogram)
                (h2 variable-binning-histogram))
  (let ((h2-content
         (variable-binning-histogram-content h2))
        (result
         (copy-hist h1)))
    (loop
       for indices being the hash-keys in h2-content
       for count being the hash-values in h2-content
       do
         (hins result indices count))
    result))

(defmethod sub ((h1 variable-binning-histogram)
                (h2 variable-binning-histogram))
  (let ((h2-content
         (variable-binning-histogram-content h2))
        (result
         (copy-hist h1)))
    (loop
       for indices being the hash-keys in h2-content
       for count being the hash-values in h2-content
       do (hins result indices (- count)))
    result))

(defmethod mult ((h1 variable-binning-histogram)
                 (h2 variable-binning-histogram))
  (let ((h1-content
         (variable-binning-histogram-content h1))
        (h2-content
         (variable-binning-histogram-content h2))
        (result
         (copy-hist h1 t)))
    (loop
       for indices being the hash-keys in h1-content
       for count1 being the hash-values in h1-content
       do (let ((count2 (gethash indices h2-content)))
            (when count2
              (hins result indices (mult count1 count2)))))
    result))

(defmethod unary-div ((h variable-binning-histogram))
  (let ((h-content
         (variable-binning-histogram-content h))
        (result
         (copy-hist h t)))
    (loop
       for indices being the hash-keys in h-content
       for count being the hash-values in h-content
       do (hins result indices (unary-div count)))
    result))

(defmethod div ((h1 variable-binning-histogram)
                (h2 variable-binning-histogram))
  (let ((h1-content
         (variable-binning-histogram-content h1))
        (h2-content
         (variable-binning-histogram-content h2))
        (result
         (copy-hist h1 t)))
    (loop
       for indices being the hash-keys in h1-content
       for count1 being the hash-values in h1-content
       do (let ((count2 (gethash indices h2-content)))
            (when count2
              (hins result indices (div count1 count2)))))
    result))

(defmethod protected-div ((h1 variable-binning-histogram)
                          (h2 variable-binning-histogram)
                          &key (protected-value 0))
  (let ((h1-content
         (variable-binning-histogram-content h1))
        (h2-content
         (variable-binning-histogram-content h2))
        (result
         (copy-hist h1 t)))
    (loop
       for indices being the hash-keys in h1-content
       for count1 being the hash-values in h1-content
       do (let ((count2 (gethash indices h2-content)))
            (when count2
              (hins result indices
                    (protected-div count1 count2
                                   :protected-value
                                   protected-value)))))
    result))
