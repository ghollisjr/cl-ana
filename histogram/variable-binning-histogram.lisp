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

(in-package :histogram)

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

(defun make-variable-binning-histogram (names-specs
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

(defmethod hist-insert ((hist variable-binning-histogram) datum &optional weight)
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
