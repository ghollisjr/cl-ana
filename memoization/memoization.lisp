;;;; memoization.lisp

;;; Memoized functions remember the previous calls of the function and
;;; look-up the return value from the last time the function was
;;; called.
;;;
;;; This implementation uses hash tables to store the previous call values.
;;;
;;; I am still unsure whether or not to expose access to the
;;; memoization hash table, at the moment it is not exposed.

(in-package :memoization)

(defvar *memoized-map* (make-hash-table :test 'equal)
  "Hash table mapping each memoized function to its value hash
  table.")

(defun get-memo-map (memo-fn)
  "Returns the memoized function's value hash table."
  (gethash memo-fn *memoized-map*))

(defmacro defun-memoized (function-name arg-list &body body)
  "Macro for defining a memoized function"
  (with-gensyms (memo-hash-table result memoed-values)
    `(let ((,memo-hash-table
            (make-hash-table :test 'equal)))
       (defun ,function-name ,arg-list
	 (let ((,memoed-values
                (multiple-value-list (gethash (list ,@arg-list)
                                              ,memo-hash-table))))
	   (if (second ,memoed-values)
	       (first ,memoed-values)
	       (let ((,result (progn ,@body)))
		 (setf (gethash (list ,@arg-list) ,memo-hash-table) ,result)
		 ,result))))
       (setf (gethash #',function-name *memoized-map*)
             ,memo-hash-table))))
