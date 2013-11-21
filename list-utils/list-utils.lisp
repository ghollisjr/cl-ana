;;;; list-utils.lisp

(in-package :list-utils)

(defun range (start end &optional (step 1))
  (labels
      ((range-helper (s e st result)
	 (if (> s e)
	     result
	     (range-helper (+ s st)
			   e
			   st
			   (cons s result)))))
    (nreverse (range-helper start end step ()))))

;; Learned this from On Lisp by Paul Graham, changed the name:
(defun singletonp (list)
  "Checks to see if list is a list with exactly one element."
  (and (consp list)
       (null (rest list))))

;; Sorry: I learned Haskell before Common Lisp, so I like
;; zipping/unzipping lists

;; Just found out that pairlis does exactly what my zip does, except
;; that (at least in sbcl) the resulting list comes in reverse order
(defun zip (x y)
  (mapcar #'cons x y))

(defun unzip (xs)
  "Returns a cons with the car being the cars of the zipped xs and the
cdr being the cdrs."
  (labels
      ((unzip-helper (xs ls rs)
	 (if xs
	     (unzip-helper (cdr xs)
			   (cons (car (car xs)) ls)
			   (cons (cdr (car xs)) rs))
	     (cons (nreverse ls) (nreverse rs)))))
    (unzip-helper xs () ())))

(defun transpose (xs)
  (labels
      ((transpose-worker (xs result)
	 (let* ((heads (remove-if #'null (mapcar #'car xs)))
		(tails (remove-if #'null (mapcar #'cdr xs))))
	   (if xs
	       (transpose-worker tails (cons heads result))
	       result))))
    (nreverse (transpose-worker xs ()))))
       
;; Can't go without the Cartesian Product
(defun cartesian-product (&rest xs)
  (if xs
      (if (singletonp xs)
	  (mapcar #'list (car xs))
	  (reduce
	   #'append
	   (mapcar
	    #'(lambda (c)
		(mapcar 
		 #'(lambda (x) (cons x c))
		 (car xs)))
	    (apply #'cartesian-product (cdr xs)))))))

;; Selects every nth term from a list (including the first term):
(defun every-nth (list n &optional (start 0))
  (labels ((every-nth-worker (list n i result)
	     ;; assumes that n > 0
	     (if list
		 (if (= i 1)
		     (every-nth-worker (rest list)
				       n
				       n
				       (cons (first list)
					     result))
		     (every-nth-worker (rest list)
				       n
				       (1- i)
				       result))
		 (when result
		   (nreverse result)))))
    (if (plusp n)
	(every-nth-worker (subseq list start) n 1 ())
	(error "Negative number given to every-nth"))))

;; Until I implement the Levi-Civita tensor again, I'm using this to
;; write certain things
(defun except-nth (x n)
  (labels
      ((except-nth-helper (x n result)
	 (cond ((> n 0)
		(except-nth-helper (cdr x) (1- n) (cons (car x) result)))
	       ((= n 0)
		(except-nth-helper (cdr x) (1- n) result))
	       ((< n 0)
		(append (reverse x) result)))))
    (nreverse (except-nth-helper x n ()))))

(defun except-at (xs ns &key (test #'eql) uniquely-sorted)
  "Yields the list of xs except for the elements at the positions in
ns.  Set uniquely-sorted to t if you want except-at to assume the ns
are already unique and sorted least to greatest."
  (labels ((except-at-worker (xs ns &optional result (current-index 0))
	     (if xs
		 (if ns
		     (if (= (first ns) current-index)
			 (except-at-worker (rest xs)
					   (rest ns)
					   result
					   (1+ current-index))
			 (except-at-worker (rest xs)
					   ns
					   (cons (first xs)
						 result)
					   (1+ current-index)))
		     ;; cons xs onto result and return result
		     (nreverse
		      (reduce (flip #'cons) xs
			      :initial-value result)))
		 (nreverse result))))
    (let ((uniquely-sorted-ns
	   (if uniquely-sorted
	       ns
	       (let ((ns-copy (copy-list ns)))
		 (reverse
		  (reduce #'(lambda (x y)
			      (adjoin y x
				      :test test))
			  (sort ns-copy #'<)
			  :initial-value ()))))))
      (except-at-worker xs uniquely-sorted-ns))))

;; Compress a list into an alist where each car is the value and each
;; cdr is the count of said value.  This is a shitty algorithm btw,
;; not useful for processing intensive stuff, but for the time being
;; it works for some of my other utilities.

(defun compress (list &key key (test #'eql) singleton-pairs)
  "Compress list such that duplicate elements are combined into a
single pair entry which has as the car the element value and as the
cdr the count.

The singleton-pairs option controls whether single elements are still
placed inside a pair.  This is useful when the elements of the list
you wish to compress are cons cells, as this would otherwise lead to
ambiguity in the result."
  (let* ((result ())
	 cons)
    (dolist (x list)
      (if (setf cons (assoc x result :key key :test test))
	  (incf (cdr cons))
	  (setf result (acons x 1 result))))
    (mapcar
     #'(lambda (x)
	 (let ((count (cdr x)))
	   (if (and (not singleton-pairs)
		    (= 1 count))
	       (car x)
	       x)))
     (nreverse result))))

(defun list-less-than (list1 list2)
  (let ((x1 (car list1))
	(x2 (car list2)))
    (cond ((and (null list1) (null list2)) nil)
	  ((null list1) t)
	  ((null list2) nil)
	  ((< x1 x2) t)
	  ((= x1 x2) (list-less-than (cdr list1) (cdr list2)))
	  (t nil))))

(defun list-greater-than (list1 list2)
  (let ((x1 (car list1))
	(x2 (car list2)))
    (cond ((and (null list1) (null list2)) nil)
	  ((null list1) nil)
	  ((null list2) t)
	  ((> x1 x2) t)
	  ((= x1 x2) (list-greater-than (cdr list1) (cdr list2)))
	  (t nil))))

(defun aref-by-list (array indexlist)
  "Access array element via index list instead of individual indices"
  (apply #'aref array indexlist))


(defun list-equal (list1 list2)
  (if (and (consp list1) (consp list2))
      (if (equal (first list1) (first list2))
	  (list-equal (rest list1) (rest list2))
	  nil)
      nil))

(defun make-offsets (sizes)
  "Takes a list of sizes and returns a list containing 0, (first
sizes), (+ (first sizes) (second sizes)), ... summing the sizes as the
list goes.

This pattern has turned out to be recurrent, and the result can be
interpreted as finding the starting index of the ith list in the
concatenation of n lists."
  (nreverse
   (rest
    (reduce #'(lambda (x y)
		(cons (+ y (car x)) x))
	    sizes
	    :initial-value (list 0)))))
