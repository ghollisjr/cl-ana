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

(in-package :cl-ana.typespec)

(declaim (optimize (speed 3)
                   (safety 1)
                   (compilation-speed 0)
                   (debug 1)))

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
;;; elements and then the size of the array along each dimension as
;;; the rest of the typespec.  Note that multidimensional array
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
;;; (:array :double 20)

;; Creates cstruct types recursively as per the typespec

(defun-memoized typespec->cffi-type (typespec)
  (labels
      ((make-slot-spec (name-cstruct)
	 (let* ((name (car name-cstruct))
		(cstruct (cdr name-cstruct))
		(type (if (typespec-array-p cstruct)
			  (typespec-array-element-type cstruct)
			  cstruct))
		(count (if (typespec-array-p cstruct)
			   (typespec-array-size cstruct)
                           1)))
           ;;(reduce #'* (fourth cstruct))))) ; array type
	   (list (keywordify (intern (string name)))
		 type
		 :count count))))
    (if (listp typespec)
	;; handle arrays and compound
	(case (first typespec)
	  ;;(:array typespec) ; not much to do
	  (:array
           (list :array
                 (typespec->cffi-type
                  (typespec-array-element-type typespec))
                 (typespec-array-size typespec)))
	  (:compound
	   ;; here's where the fun happens
	   (let* ((names (typespec-compound-field-names typespec))
		  (specs (typespec-compound-field-specs typespec))
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

;;;; Compound utilities:

(defun typespec-compound-p (typespec)
  "Tests a typespec for being a compound typespec."
  (when (consp typespec)
    (equal (first typespec) :compound)))

(defun typespec-compound-field-alist (typespec)
  "Returns the alist mapping field name to typespec."
  (when (typespec-compound-p typespec)
    (rest typespec)))

(defun typespec-compound-field-names (typespec)
  "Returns the field names for the compound type."
  (when (typespec-compound-p typespec)
    (mapcar #'car
            (typespec-compound-field-alist typespec))))

(defun typespec-compound-field-specs (typespec)
  "Returns the typespecs for the fields in the compound type."
  (when (typespec-compound-p typespec)
    (mapcar #'cdr
            (typespec-compound-field-alist typespec))))

;;;; Array utilities:

(defun typespec-array-p (typespec)
  "Tests a typespec for being an array typespec."
  (when (consp typespec)
    (equal (first typespec) :array)))

(defun typespec-array-element-type (typespec)
  "Returns the element type for the array type."
  (when (typespec-array-p typespec)
    (second typespec)))

(defun typespec-array-dim-list (typespec)
  "Returns the list of dimension sizes for the array type."
  (when (typespec-array-p typespec)
    (rest (rest typespec))))

(defun typespec-array-rank (typespec)
  "Returns the number of dimensions (rank) of the array type."
  (when (typespec-array-p typespec)
    (length (typespec-array-dim-list typespec))))

(defun typespec-array-size (typespec)
  "Returns the total number of elements in the array type."
  (when (typespec-array-p typespec)
    (reduce #'*
            (typespec-array-dim-list typespec))))

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
	   (list :array
		 (typespec-flatten-arrays
                  (typespec-array-element-type typespec))
		 (typespec-array-size typespec)))
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
                 for field-spec in (typespec-compound-field-specs typespec)
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
       (let ((element-type (typespec-array-element-type
                            typespec))
             (array-size (typespec-array-size typespec)))
         (if (equal element-type :char)
             ;; handle strings:
             (let ((field-setter (typespec->lisp-to-c element-type)))
               (lambda (tensor c-pointer)
                 (let ((element-converter
                        (if (typep tensor 'string)
                            #'char-code
                            #'identity)))
                   (dotimes (i array-size)
                     (funcall field-setter
                              (funcall element-converter (tensor-flat-ref tensor i))
                              (mem-aptr c-pointer
                                        element-type
                                        i))))))
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
            for (field-name . field-spec)
            in (typespec-compound-field-alist typespec)
            collecting (keywordify (string field-name))
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
     (let* ((element-type (typespec-array-element-type typespec))
            (dim-list (typespec-array-dim-list typespec))
            ;; these should be used whenever the lisp array version is used:
            ;; (dim-vector (coerce dim-list 'vector))
            ;; (ndims (length dim-list))
            (num-elements (typespec-array-size typespec))
            (element-cffi-type
             (typespec->cffi-type element-type))
            (element-getter
             (typespec->c-to-lisp element-type)))
       ;; function which returns an array of appropriate dimensions
       ;; with the foreign content.

       ;; ;; This function is here as an example of how to do this
       ;; ;; with lisp arrays, faster than using the tensors but at
       ;; ;; the cost of the tensor functions.
       ;; 
       ;; (lambda (c-pointer)
       ;;   (let ((result-array
       ;;          (make-array dim-list))
       ;;         (indices (make-array (list (length dim-vector))
       ;;                              :initial-element 0)))
       ;;     (labels ((inc-indices! ()
       ;;                (inc-indices-worker! (1- ndims)))
       ;;              (inc-indices-worker! (inc-index)
       ;;                (if (>= (elt indices inc-index)
       ;;                        (1- (elt dim-vector inc-index)))
       ;;                    (when (plusp inc-index)
       ;;                      (setf (elt indices inc-index) 0)
       ;;                      (inc-indices-worker! (1- inc-index)))
       ;;                    (incf (elt indices inc-index)))))
       ;;       (loop
       ;;          for i below num-elements
       ;;          do (progn
       ;;               (setf (apply #'aref result-array (coerce indices 'list))
       ;;                     (funcall element-getter
       ;;                              (mem-aptr c-pointer
       ;;                                        element-cffi-type
       ;;                                        i)))
       ;;               (inc-indices!)))
       ;;       result-array)))
       
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
           result-tensor))
       ))
    ;; primitive
    (t
     (lambda (c-pointer)
       (mem-aref c-pointer typespec)))))

(defun char-vector->string (char-vector &optional length)
  "Returns a string version of a vector of integers interpreted as the
numerical codes for characters.  If argument length is given, the
result contains only the first length characters, or if length is
longer than the char-vector, the char-vector interpreted into a string
as if length were not given."
  (let* ((length (if length
                     (min length (length char-vector))
                     (length char-vector)))
         (result (make-string length)))
    (loop
       for i from 0 below length
       for c across char-vector
       do (setf (elt result i)
                (int-char c)))
    result))
