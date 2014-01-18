;;;; typespec.lisp

(in-package :typespec)

(declaim (optimize (speed 3)
                   (safety 0)
                   (compilation-speed 0)
                   (debug 0)))

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
;;; (Note that strings are difficult: C treats character arrays as
;;; strings, and it is farirly seemless to convert between them;
;;; additionally small integers are routinely encoded as characters.
;;; However: Lisp's CFFI treats them differently, and so I have
;;; implemented the following conventions:
;;;
;;; *** Character arrays are converted to Lisp as vectors of integers.
;;; You can apply char-vector->string to convert this raw character
;;; array into a string.
;;;
;;; *** String typespecs should be that of a character array; this is
;;; necessary since most binary storage requires a fixed size for the
;;; data type.  The same treatment above is necessary when the string
;;; is being read back into Lisp.)
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
		(type (if (typespec-array-p cstruct)
			  (second cstruct)
			  cstruct))
		(count (if (typespec-array-p cstruct)
			   (third cstruct)
                           1)))
           ;;(reduce #'* (fourth cstruct))))) ; array type
	   (list (keywordify (intern (lispify name)))
		 type
		 :count count))))
    (if (listp typespec)
	;; handle arrays and compound
	(case (first typespec)
	  ;;(:array typespec) ; not much to do
	  (:array
	   (append (list (first typespec))
		   (list (typespec->cffi-type (second typespec)))
		   ;;(rest (rest typespec))))
                   (list (reduce #'* (fourth typespec)))))
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
  (when (consp typespec)
    (equal (first typespec) :compound)))

(defun typespec-array-p (typespec)
  "Tests a typespec for being an array typespec."
  (when (consp typespec)
    (equal (first typespec) :array)))

;; This is for automatically translating vectors into foreign arrays;
;; only works for 1-D at the moment but that is ok.  Note that I had
;; to look into the CFFI code to get to this solution, so if they
;; change it significantly this may cease to work.
;; (defmethod translate-to-foreign ((value simple-vector) type)
;;   (let* ((size (reduce #'* (cffi::dimensions type)))
;;          (element-type (cffi::element-type type))
;;          (result (foreign-alloc element-type :count size)))
;;     (loop
;;        for i below size
;;        do (setf (mem-aref result element-type i)
;;                 (elt value i)))
;;     result))

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
		  (lambda (cons)
                    (cons (car cons)
                          (typespec-flatten-arrays (cdr cons))))
		  (rest typespec)))))
	typespec)))

(defun typespec-foreign-alloc (typespec &optional (count 1))
  "Allocates space for count foreign objects of type specified by
typespec."
  (foreign-alloc (typespec->cffi-type typespec) :count count))

(defun typespec->lisp-to-c (typespec)
  "Returns a function which takes

1. a lisp object,
2. a corresponding C pointer

and sets the fields/values of the C object recursively.  The pointer
to the C-object is returned."
  (let ((cstruct (typespec->cffi-type typespec)))
    (cond
      ;; Compound types:
      ((typespec-compound-p typespec)
       (let ((field-setters
              (loop
                 for (_ . field-spec) in (rest typespec)
                 collect (typespec->lisp-to-c field-spec))))
         (lambda (plist c-pointer)
           (do* ((lst plist (rest (rest lst)))
                 (fs (first lst) (first lst))
                 (fv (second lst) (second lst))
                 (setter-lst field-setters (rest setter-lst))
                 (field-setter (first setter-lst)
                               (first setter-lst)))
                ((or (null lst)
                     (null setter-lst))
                 c-pointer)
             (funcall field-setter
                      fv
                      (foreign-slot-pointer c-pointer
                                            cstruct
                                            fs))))))
      ;; Array types:
      ((typespec-array-p typespec)
       (destructuring-bind (element-type
                            array-size)
           (rest (typespec->cffi-type typespec))
         (if (equal element-type :char)
             ;; handle strings:
             (let ((field-setter (typespec->lisp-to-c element-type)))
               (lambda (tensor c-pointer)
                 (dotimes (i array-size)
                   (funcall field-setter
                            (char-code (tensor-flat-ref tensor i))
                            (mem-aptr c-pointer
                                      element-type
                                      i)))))
             ;; other arrays:
             (let ((field-setter (typespec->lisp-to-c element-type)))
               (lambda (tensor c-pointer)
                 (dotimes (i array-size)
                   (funcall field-setter
                            (tensor-flat-ref tensor i)
                            (mem-aptr c-pointer
                                      element-type
                                      i))))))))
      ;; Primitive types:
      (t
       (lambda (primitive c-pointer)
         (setf (mem-aref c-pointer cstruct)
               primitive))))))

(defun typespec->c-to-lisp (typespec)
  "Returns a function which takes a c-pointer argument and returns a
lisp object containing the converted values."
  (cond
    ;; compound
    ((typespec-compound-p typespec)
     (multiple-value-bind (field-symbols field-getters)
         (loop
            for (field-name . field-spec) in (rest typespec)
            collecting (intern (lispify field-name))
            into field-symbols
            collecting (typespec->c-to-lisp field-spec)
            into field-getters
            finally (return (values field-symbols
                                    field-getters)))
       (let ((cstruct (typespec->cffi-type typespec)))
         (lambda (c-pointer)
           (loop
              for fs in field-symbols
              for fg in field-getters
              collecting fs into result
              collecting
                (funcall fg
                         (foreign-slot-pointer c-pointer
                                               cstruct
                                               fs))
              into result
              finally
                (return result))))))
    ;; array
    ((typespec-array-p typespec)
     (let* ((element-type (second typespec))
            (dim-list (fourth typespec))
            (num-elements (reduce #'* dim-list))
            (element-cffi-type
             (typespec->cffi-type element-type))
            (element-getter
             (typespec->c-to-lisp element-type)))
       (lambda (c-pointer)
         (let ((result-tensor
                (make-tensor dim-list)))
           (loop
              for i below num-elements
              do (setf (tensor-flat-ref result-tensor i)
                       (funcall element-getter
                                (mem-aptr c-pointer
                                          element-cffi-type
                                          i))))
           result-tensor))))
    ;; primitive
    (t
     (lambda (c-pointer)
       (mem-aref c-pointer typespec)))))

(defun char-vector->string (char-vector)
  (let ((result (make-string (length char-vector))))
    (loop
       for i from 0
       for c across char-vector
       do (setf (elt result i)
                (int-char c)))
    result))
