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

(defmacro defun-memoized (function-name arg-list &body body)
  "Macro for defining a memoized function"
  (let ((memo-hash-table (gensym))
	(result (gensym)))
    `(progn
       (defvar ,memo-hash-table (make-hash-table :test 'equal))
       (defun ,function-name ,arg-list
	 (let ((memoed-values (multiple-value-list (gethash (list ,@arg-list) ,memo-hash-table))))
	   (if (second memoed-values)
	       (first memoed-values)
	       (let ((,result (progn ,@body)))
		 (setf (gethash (list ,@arg-list) ,memo-hash-table) ,result)
		 ,result)))))))
