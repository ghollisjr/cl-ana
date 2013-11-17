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
;;; Container types:
;;;
;;; :compound would denote that the type is a structure and the
;;; subsequent elements would be lists of the names and typespecs of
;;; the elements.  (This is just the symbol :compound consed onto an
;;; alist mapping names to typespecs.)
;;;
;;; :array would denote that the type is an array type.  The following
;;; elements in the typespec list are the typespec of the array
;;; elements, the rank of the array, and a list containing the size of
;;; the array along each dimension.  Note that multidimensional array
;;; typespecs need to be flattened if you intend to use
;;; convert-from-foreign.  The function flatten-array-typespec is
;;; provided for this purpose.
;;;
;;; Example 1: A structure with integer x and float y:
;;; (:compound ("x" . :int) ("y" . :float))
;;;
;;; Example 2: A 1-D array of 20 doubles:
;;; (:array :double 1 (20))

;; Creates cstruct types recursively as per the typespec
(defun-memoized typespec->cffi-type (typespec)
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
		   (list (typespec->cffi-type (second typespec)))
		   (rest (rest typespec))))
	  (:compound
	   ;; here's where the fun happens
	   (let* ((names-specs (rest typespec))
		  (names (mapcar #'car names-specs))
		  (specs (mapcar #'cdr names-specs))
		  (member-cstructs (mapcar #'typespec->cffi-type specs))
		  (names-cstructs (zip names member-cstructs))
		  (slotspecs (mapcar #'make-slot-spec names-cstructs)))
	     ;; I'd like to find a better way to do this, but I have
	     ;; to evaluate slotspecs and gensym and splice them into
	     ;; code which will be evaluated, so a simple macro
	     ;; doesn't seem to work.
	     (eval
	      `(defcstruct ,(gensym) ,@slotspecs)))))
	  typespec))) ; since the symbol types are already known to cffi

(defun typespec-compound-p (typespec)
  "Tests a typespec for being a compound typespec."
  (equal (first typespec) :compound))

(defun typespec-array-p (typespec)
  "Tests a typespec for being an array typespec."
  (equal (first typespec) :array))

(defun typespec-flatten-arrays (typespec)
  "Flattens an array typespec if typespec is an array typespec,
otherwise returns typespec unchanged.  Note that this operation
is recursive, in that if there are arrays in the typespec(s) of the
component type(s), then they are flattened as well."
  (when typespec
    (if (listp typespec)
	(cond
	  ((typespec-array-p typespec)
	   (list (first typespec)
		 (typespec-flatten-arrays (second typespec))
		 (apply #'* (first (last typespec)))))
	  ((typespec-compound-p typespec)
	   (cons :compound
		 (mapcar
		  #'(lambda (cons)
		      (cons (car cons)
			    (typespec-flatten-arrays (cdr cons))))
		  (rest typespec)))))
	typespec)))
