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
      (if (single xs)
	  (mapcar #'list (car xs))
          (mapcan
           (lambda (c)
             (mapcar
              (lambda (x) (cons x c))
              (car xs)))
           (apply #'cartesian-product (cdr xs))))))

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
		  (reduce (lambda (x y)
                            (adjoin y x
                                    :test test))
			  (sort ns-copy #'<)
			  :initial-value ()))))))
      (except-at-worker xs uniquely-sorted-ns))))

;; Compress a list into an alist where each car is the value and each
;; cdr is the count of said value.  This is a shitty algorithm btw,
;; not useful for processing intensive stuff, but for the time being
;; it works for some of my other utilities.

(defun compress (list &key
                        (key #'identity)
                        (test 'eql)
                        sort-by
                        singleton-pairs)
  "Compress list such that duplicate elements are combined into a
single pair entry which has as the car the element value and as the
cdr the count.

The singleton-pairs option controls whether single elements are still
placed inside a pair.  This is useful when the elements of the list
you wish to compress are cons cells, as this would otherwise lead to
ambiguity in the result."
  (let* ((result-table (make-hash-table :test test)))
    (dolist (x list)
      (if (gethash x result-table)
          (incf (gethash x result-table))
          (setf (gethash x result-table) 1)))
    (let* ((map-fn (if singleton-pairs
                       (lambda (x c)
                         (cons x c))
                       (lambda (x c)
                         (if (= 1 x)
                             x
                             (cons x c)))))
           (raw-alist
            (loop
               for k being the hash-keys of result-table
               for v being the hash-values of result-table
               collect (funcall map-fn k v))))
      (if sort-by
          (sort raw-alist sort-by
                :key (if singleton-pairs
                         (lambda (x)
                           (funcall key (car x)))
                         (lambda (x)
                           (if (consp x)
                               (funcall key (car x))
                               (funcall key x)))))
          raw-alist))))

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
    (reduce (lambda (x y)
              (cons (+ y (car x)) x))
	    sizes
	    :initial-value (list 0)))))

;;; Stuff (mostly) from Paul Graham's On Lisp, as well as some of my
;;; own improvements.

(declaim (inline single append1 conc1 mklist))

;; Would include last1, but Alexandria has last-elt, which is last1
;; for sequences

(defun length-equal (list length)
  "Returns t/nil as soon as it is apparent that the list does not
contain exactly length elements."
  (declare (integer length))
  (labels ((rec (list length)
             (if (zerop length)
                 (if (null list)
                     t
                     nil)
                 (if (consp list)
                     (length-equal (cdr list) (1- length))
                     nil))))
    (if (< length 0)
        nil
        (rec list length))))

(defun single (list)
  "Checks to see if list is a list with exactly one element."
  (and (consp list)
       (null (rest list))))

(defun append1 (list object)
  "Creates a list containing object and appends list to it."
  (append list (list object)))

(defun conc1 (list object)
  "Like append1, but uses nconc instead of append"
  (nconc list (list object)))

(defun mklist (object)
  "Ensures that object is a list, if not object is packaged in a
list."
  (if (listp object)
      object
      (list object)))

(defun longer (x y)
  "Efficiently compares two lists, or if they're not both lists simply
calls length on each and compares.  This could be sped up a bit if one
argument is a list and the other not, so it's a work in progress."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

;; filter is (IMO) not so necessary (as Paul Graham uses the term)

(defun group (source n)
  "Takes a list and returns a list of lists of the elements in the
list grouped in lists of size n with the remainder in the last list."
  (if (zerop n)
      (error "zero length")
      (labels ((group-worker (source acc)
                 (let ((rest (nthcdr n source)))
                   (if (consp rest)
                       (group-worker rest
                                     (cons (subseq source 0 n) acc))
                       (nreverse (cons source acc))))))
        (if source
            (group-worker source nil)
            nil))))

;; flatten is already provided by Alexandria.  To reduce a tree just
;; flatten the list first and then reduce it.

(defun prune (test tree)
  "Like remove-if, but for lists treated as trees."
  (labels ((prune-worker (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (prune-worker
                     (cdr tree)
                     (cons (prune-worker (car tree) nil) acc)))
                   (t (prune-worker (cdr tree)
                                    (if (funcall test (car tree))
                                        acc
                                        (cons (car tree) acc)))))))
    (prune-worker tree nil)))

(defun find2 (fn list)
  "Like find, but it returns two values: the list element and the
value that fn returned."
  (if (null list)
      nil
      (let ((val (funcall fn (car list))))
        (if val
            (values (car list) val)
            (find2 fn (cdr list))))))

(defun before (x y list &key (test #'eql))
  "Tells you if x is found before y in list."
  (and list
       (let ((first (car list)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) list)
               (t (before x y (cdr list) :test test))))))

(defun after (x y list &key (test #'eql))
  "Tells you if x is found after y in list."
  (let ((rest (before y x list :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (object list &key (test #'eql))
  "Tells you if the object occurs more than once in the list."
  (member object (cdr (member object list :test test))
          :test test))

;; split-if is provided by split-sequence:split-sequence-if

;; Operator for looping over a plist:
(defmacro do-plist ((field-symbol field-value plist) &body body)
  "Executes body via looping over plist, binding each field symbol to
field-symbol and each field value to field-value.  Returns nil."
  (with-gensyms (lst)
    `(do ((,lst ,plist (rest (rest ,lst))))
         ((null ,lst))
       (let ((,field-symbol (first ,lst))
             (,field-value (second ,lst)))
         ,@body))))

(defun permute (list permutation)
  "Re-arranges the elements in list according to the permutation."
  (mapcar (lambda (p)
            (elt list p))
          permutation))
