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

(in-package :cl-ana.binary-tree)

;;; Binary Tree: (value left-child right-child numvalues)

(defun node-leaf-p (node)
  (not (or (node-left-child node)
           (node-right-child node))))

(defun node-value (tree)
  (first tree))

(defun node-left-child (tree)
  (second tree))

(defun node-right-child (tree)
  (third tree))

(defun node-value-count (tree)
  (fourth tree))

(defun make-balanced-tree (list &key
				  key
				  sorted
				  (comparison #'<)
				  (test #'equal))
  (if sorted
      (make-balanced-tree-sorted-compressed
       (compress list :key key :test test :singleton-pairs t))	
      (make-balanced-tree-sorted-compressed
       (compress (sort (copy-list list) comparison :key key)
		 :key key :test test :singleton-pairs t))))

(defun make-balanced-tree-sorted-compressed (list)
  "Assumes the list is sorted & compressed with singleton-pairs
enabled."
  (when list
    (let* ((median-position (median-position list))
	   (median (elt list median-position))
	   (leftnodes
	    (if (zerop median-position)
		nil
		(subseq list 0 median-position)))
	   (rightnodes
	    (subseq list (1+ median-position))))
      (list (car median)
	    (make-balanced-tree-sorted-compressed leftnodes)
	    (make-balanced-tree-sorted-compressed rightnodes)
	    (cdr median)))))
      
(defun compress-equal (x y)
  (labels
      ((get-value (z)
	 (if (consp z)
	     (car z)
	     z)))
    (let ((xvalue (get-value x))
	  (yvalue (get-value y)))
      (equal xvalue yvalue))))

(defun median-position (list)
  (let ((length (length list)))
    (if (= 1 length)
	0
	(1- (floor (/ length 2))))))

;;;; Access functions:

(defun bref (tree val &key (key #'identity))
  "Returns a cons pair of node values which form the most constraining
interval around val using key."
  (labels ((rightmost (tree)
             (if (node-leaf-p tree)
                 (node-value tree)
                 (rightmost (node-right-child tree))))
           (leftmost (tree)
             (if (node-leaf-p tree)
                 (node-value tree)
                 (leftmost (node-left-child tree))))
           (rec (tree val &optional low hi)
             (let* ((tval (node-value tree))
                    (kval (funcall key tval)))
               (if (node-leaf-p tree)
                   (if (= kval val)
                       (values tval t)
                       (values
                        (cons (if (< kval val)
                                  tval
                                  low)
                              (if (> kval val)
                                  tval
                                  hi))
                        nil))
                   (cond
                     ((= kval val)
                      (values tval t))
                     ((> kval val)
                      (let ((left (node-left-child tree)))
                        (if left
                            (rec (node-left-child tree)
                                 val
                                 low
                                 tval)
                            (values (cons low tval) nil))))
                     (t
                      (let ((right (node-right-child tree)))
                        (if right
                            (rec (node-right-child tree)
                                 val
                                 tval
                                 hi)
                            (values (cons tval hi) nil)))))))))
    (rec tree val)))
