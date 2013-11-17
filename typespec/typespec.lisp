;;;; typespec.lisp

(in-package :typespec)

;;; Typespec: A typespec (as I'm referring to it) is either
;;;
;;; 1. A symbol denoting the cffi type
;;;
;;; 2. A list with the first element denoting the container type and
;;; the subsequent elements describing the members of the container
;;; type.
;;;
;;; For example, :compound would denote that the type is a structure
;;; and the subsequent elements would be lists of the names and
;;; typespecs of the elements.  (This is just the symbol :compound
;;; consed onto an alist mapping names to typespecs.) Or :array would
;;; denote that the type is an array type and the second and third
;;; elements would give the typespec of the base type and the number
;;; of elements respectively.
;;;
;;; Example 1: A structure with integer x and float y:
;;; (:compound ("x" . :int) ("y" . :float))
;;;
;;; Example 2: A 1-D array of 20 doubles:
;;; (:array :double 1 (20))

;; Creates cstruct types recursively as per the typespec
(defun-memoized typespec->cstruct (typespec)
  (labels
      ((make-slot-spec (name-cstruct)
	 (let* ((name (car name-cstruct))
		(cstruct (cdr name-cstruct))
		(type (if (symbolp cstruct)
			  cstruct
			  (second cstruct))) ; array type
		(count (if (symbolp cstruct)
			   1
			   (reduce #'* (fourth cstruct))))) ; array type
	   (list (intern (lispify name))
		 type
		 :count count))))
    (if (listp typespec)
	;; handle arrays and compound
	(case (first typespec)
	  ;;(:array typespec) ; not much to do
	  (:array
	   (append (list (first typespec))
		   (list (typespec->cstruct (second typespec)))
		   (rest (rest typespec))))
	  (:compound
	   ;; here's where the fun happens
	   (let* ((names-specs (rest typespec))
		  (names (mapcar #'car names-specs))
		  (specs (mapcar #'cdr names-specs))
		  (member-cstructs (mapcar #'typespec->cstruct specs))
		  (names-cstructs (zip names member-cstructs))
		  (slotspecs (mapcar #'make-slot-spec names-cstructs)))
	     (eval
	      `(defcstruct ,(gensym) ,@slotspecs)))))
	  typespec))) ; since the symbol types are already known to cffi
