(in-package :binary-tree)

;;; Binary Tree: (value left-child right-child numvalues)

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
