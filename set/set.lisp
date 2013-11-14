;;;; set.lisp

;;; This is an implementation of the set datatype using hash tables; I
;;; simply add a member to the hash table with the mapped value as
;;; t.

;;; NOTE: The set-add and set-remove functions do change the set object, so if you want to keep your old set object, copy it with copy-set

(in-package :set)

(defun make-set (&key initial-sequence (test 'eq))
  "Creates a set with hash tables as the implementation.  This means
that test should be a symbol correspond to eq, equal, or the other
allowed hash table tests."
  (reduce (flip #'set-add) initial-sequence :initial-value (make-hash-table :test test)))

(defun mapset (f s)
  "Maps the function f over the set s.  Note that this results in a
new set, so repeat values are not stored in the result."
  (let ((new-set (make-set :test (hash-table-test s))))
    (loop for k being the hash-keys in s
	 do (set-add (funcall f k) new-set))
    new-set))

(defmacro doset ((element set) &body body)
  "Loop over set values much like dolist"
  `(loop for ,element being the hash-keys in ,set
	do (progn ,@body)))

(defun set-add (v s)
  "Adds the value v to the set s if it's not already present"
  (if (gethash v s)
      s
      (setf (gethash v s) t))
  s)

(defun set-remove (v s)
  "Removes the value v from the set s if it's present"
  (remhash v s))

(defun set-member (v s)
  "Checks whether or not the value v is in the set s"
  (gethash v s))

(defun copy-set (set)
  "Copies set into a new set object"
  (let ((new-set (make-set :test (hash-table-test set))))
    (loop for k being the hash-keys in set
	 do (set-add k new-set))
    new-set))
