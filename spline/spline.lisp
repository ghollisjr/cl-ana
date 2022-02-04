;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2021 Gary Hollis
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

(in-package :cl-ana.spline)

;; This project supports natural splines of any order, uniform and
;; non-uniform.

;;; Adding basic FFI for GSL's sparse matrix functions.
(defparameter +GSL-CONTINUE+ -2) ; from gsl_errno.h
(defparameter +GSL-SUCCESS+ 0) ; from gsl_errno.h

(defun GSL-ITERSOLVE-GMRES ()
  (cffi:foreign-symbol-pointer
   "gsl_splinalg_itersolve_gmres"))

;; I want to use this, but on my system this causes strange memory
;; bugs.
;;
;; (cffi:use-foreign-library gsll::libgsl)
;;
;; So instead I do this:
(cffi:define-foreign-library gsl
  (:windows (:or "libgsl-0.dll" "cyggsl-0.dll"))
  (:darwin
       (:or (:framework "libgsl") "libgsl.dylib"))
  (:unix (:or
          "libgsl.so.25"
          "libgsl.so.0" "libgsl.so"))
  (t (:default "libgsl")))

(cffi:use-foreign-library gsl)

;; Vectors
(cffi:defcfun "gsl_vector_alloc" :pointer
  (nelements :int))
(cffi:defcfun "gsl_vector_free" :void
  (vector :pointer))
(cffi:defcfun "gsl_vector_get" :double
  (vector :pointer)
  (i :int))
(cffi:defcfun "gsl_vector_set" :void
  (vector :pointer)
  (i :int)
  (x :double))
(cffi:defcfun "gsl_vector_ptr" :pointer
  (vector :pointer)
  (i :int))
(cffi:defcfun "gsl_vector_set_zero" :void
  (vector :pointer))
(cffi:defcfun "gsl_vector_set_all" :void
  (vector :pointer)
  (x :double))
(cffi:defcfun "gsl_vector_set_basis" :void
  (vector :pointer)
  (i :int))

;; Sparse Matrices
(cffi:defcfun "gsl_spmatrix_alloc" :pointer
  (nrows :unsigned-int)
  (ncols :unsigned-int))
(cffi:defcfun "gsl_spmatrix_free" :void
  (matrix :pointer))
(cffi:defcfun "gsl_spmatrix_get" :double
  (matrix :pointer)
  (i :int)
  (j :int))
(cffi:defcfun "gsl_spmatrix_set" :int
  (matrix :pointer)
  (i :int)
  (j :int)
  (x :double))
(cffi:defcfun "gsl_spmatrix_set_zero" :int
  (matrix :pointer)
  (i :int)
  (j :int))
(cffi:defcfun "gsl_spmatrix_ptr" :pointer
  (matrix :pointer)
  (i :int)
  (j :int))
;; compress matrix
(cffi:defcfun "gsl_spmatrix_ccs" :pointer
  (matrix :pointer))
;; Linear algebra
;; returns workspace
(cffi:defcfun "gsl_splinalg_itersolve_alloc" :pointer
  (type :pointer)
  (n :int)
  (m :int))
(cffi:defcfun "gsl_splinalg_itersolve_free" :void
  (workspace :pointer))
(cffi:defcfun "gsl_splinalg_itersolve_name" :pointer
  (workspace :pointer))
(cffi:defcfun "gsl_splinalg_itersolve_iterate" :int
  (sparse-matrix :pointer)
  (rhs-vector :pointer)
  (tolerance :double)
  (result-vector :pointer)
  (workspace :pointer))
(cffi:defcfun "gsl_splinalg_itersolve_normr" :double
  (workspace :pointer))

;; Utility/Helper Functions
(defun make-splinalg-workspace (nrows &optional (subspacerows 0))
  (gsl-splinalg-itersolve-alloc (cffi:mem-ref (GSL-ITERSOLVE-GMRES)
                                              :pointer 0)
                                nrows
                                subspacerows))

(defun sp-solve-system (coefs vector)
  (let* ((nrows (length coefs))
         (ncols (length (first coefs)))
         (mat (gsl-spmatrix-alloc nrows ncols))
         (vec (gsl-vector-alloc (length vector)))
         (w (make-splinalg-workspace nrows nrows))
         (tol 1d-3)
         (res (gsl-vector-alloc (length vector))))
    (loop
       for i from 0
       for row in coefs
       do
         (loop
            for j from 0
            for x in row
            when (not (zerop x))
            do (gsl-spmatrix-set mat i j x)))
    (loop
       for v in vector
       for i from 0
       do (gsl-vector-set vec i v))
    (let* ((m (gsl-spmatrix-ccs mat))
           (stat +GSL-CONTINUE+))
      (loop
         while (= stat +GSL-CONTINUE+)
         do (setf stat
                  (gsl-splinalg-itersolve-iterate m vec
                                                  tol
                                                  res
                                                  w))))
    (loop
       for i below (length vector)
       collecting (gsl-vector-get res i))))

(defstruct polynomial-spline
  degree ;integer
  coefs ;2-D array
  xs ;vector
  deltas ;vector
  )

(defun spline-bin-index (xs x)
  "Returns integer index to spline bin"
  (loop
     for i from 0
     for xlow across xs
     when (> xlow x)
     do (return (1- i))
     finally (return (1- (length xs)))))

(defun evaluate-polynomial-spline
    (spline x
     &key
       ;; set to T to maintain final value outside
       ;; domain.  Default is to have value 0
       ;; outside domain.
       continued-boundary-p)
  (with-slots (coefs deltas xs degree) spline
    (labels ((ev (index x)
               (polynomial (loop
                              for j to degree
                              collecting (aref coefs index j))
                           (/ (- x (aref xs index))
                              (aref deltas index)))))
      (let* ((index (spline-bin-index xs x)))
        (cond
          ((minusp index)
           (if continued-boundary-p
               (ev 0 (aref xs 0))
               0d0))
          ((> index (- (length xs) 2))
           (if continued-boundary-p
               (ev (- (length xs) 2)
                   (aref xs (1- (length xs))))
               0d0))
          (t
           (ev index x)))))))

(defun polynomial-derivative (params x degree)
  "Evaluates derivative of given degree of polynomial at point x."
  (polynomial (loop
                 for i from 0
                 for p in params
                 when (>= i degree)
                 collecting (* (npermutations i degree) p))
              x))

(defun polynomial-integral (params xlo xhi)
  "Evaluates definite integral of polynomial."
  (flet ((pint (x)
           (polynomial (cons 0d0
                             (loop
                                for i from 1d0
                                for p in params
                                collecting (/ p i)))
                       x)))
    (- (pint xhi) (pint xlo))))

(defun evaluate-polynomial-spline-derivative (spline x deg)
  (with-slots (coefs deltas xs degree) spline
    (labels ((ev (index x)
               (* (expt (aref deltas index) deg)
                  (polynomial-derivative
                   (loop
                      for j to degree
                      collecting (aref coefs index j))
                   (/ (- x (aref xs index))
                      (aref deltas index))
                   deg))))
      (let* ((index (spline-bin-index xs x)))
        (cond
          ((minusp index)
           0d0)
          ((> index (- (length xs) 2))
           0d0)
          (t
           (ev index x)))))))

(defun evaluate-polynomial-spline-integral (spline xlo xhi)
  "Evaluates definite integral of natural spline."
  (with-slots (xs deltas coefs degree) spline
    (labels ((binparams (xbin)
               (loop
                  for i to degree
                  collecting (aref coefs xbin i)))
             (wholebinint (xbin)
               (let* ((params (binparams xbin)))
                 (* (aref deltas xbin)
                    (polynomial-integral params 0d0 1d0)))))
      (let* ((N (1- (length xs)))
             (sign (cond
                     ((< xlo xhi)
                      1d0)
                     ((> xlo xhi)
                      -1d0)
                     (t 0d0)))
             (xlo (if (minusp sign)
                      xhi
                      xlo))
             (xhi (if (minusp sign)
                      xlo
                      xhi))
             (xlowbin
              (awhen (loop
                        for i from 0
                        for x across xs
                        when (>= x xlo)
                        do (return (1- i))
                        finally (return nil))
                (max it
                     0)))
             (xlowoffset (when xlowbin
                           (max (- xlo (aref xs xlowbin)) 0d0)))
             (xhighbin
              (awhen (loop
                        for i downfrom (- N 1) downto 0
                        for x = (aref xs i)
                        when (< x xhi)
                        do (return i)
                        finally (return nil))
                (min (- N 1)
                     it)))
             (xhighoffset (when xhighbin
                            (min (- xhi (aref xs xhighbin))
                                 (aref deltas (1- N)))))
             (lowparams (when xlowbin (binparams xlowbin)))
             (highparams (when xhighbin (binparams xhighbin))))
        (if (and xlowbin
                 xhighbin
                 (= xlowbin xhighbin))
            (* (aref deltas xlowbin)
               (polynomial-integral lowparams
                                    xlowoffset
                                    xhighoffset))
            (let* ((lowint
                    (when xlowbin
                      (* (aref deltas xlowbin)
                         (polynomial-integral lowparams
                                              (/ (- xlowoffset
                                                    (aref xs xlowbin))
                                                 (aref deltas xlowbin))
                                              1d0))))
                   (highint
                    (when xhighbin
                      (* (aref deltas xhighbin)
                         (polynomial-integral highparams
                                              0d0
                                              (/ xhighoffset
                                                 (aref deltas xhighbin)))))))
              (when (and lowint highint)
                (+ lowint
                   highint
                   (loop
                      for i
                      from (1+ xlowbin)
                      to (1- xhighbin)
                      summing
                        (wholebinint i))))))))))

(defun polynomial-spline-constraint
    (poly-degree npoints bin value
     &key
       (side :left) ; can be :right
       (derivative 1))
  "Generates a constraint list for the polynomial-spline function.
Poly-degree sets the polynomial degree, nbins specifies the number of
points in the spline, bin determines the spline polynomial to
constrain (0 through npoints-2), value sets the value for the RHS
vector, and derivative selects the degree of derivative which the
constraint applies to.

This can be used to create more complex constraints by using value=0
and adding together whatever linear combinations of constraints you
need, finally setting the last element to whatever constraint value
you need.  Set side to :right to constrain a polynomial using the
right point of the bin rather than the left point.

CAUTION: You can use this function to generate nonsensical constraints
that are incompatible with the rest of the spline, which will lead to
failure to solve the system of equations.  Derivatives are generally
safe to set, whereas values are already constrained by the nature of
the spline, which is why the derivative argument defaults to 1 rather
than 0."
  (let* ((nbins (1- npoints))
         (ncoefs (* (1+ poly-degree)
                    nbins))
         (result (make-array (1+ ncoefs) :initial-element 0d0)))
    (case side
      (:left
       (setf (aref result
                   (+ derivative
                      (* (1+ poly-degree)
                         bin)))
             (->double-float (factorial derivative))))
      (:right
       (loop
          for i from derivative to poly-degree
          for ii = (+ (* (1+ poly-degree)
                         bin)
                      i)
          do (setf (aref result ii)
                   (->double-float
                    (npermutations i derivative))))))
    (setf (aref result ncoefs) value)
    (coerce result 'list)))

(defun polynomial-spline (points
                          &key
                            derivatives
                            constraints
                            (degree 3)
                            (tolerance 1d-5))
  "Creates polynomial spline of arbitrary degree and adjustable
derivative constraints.

Polynomial splines have most degrees of freedom constrained by
requiring continuity of all but one non-zero derivative.  Natural
splines set the remaining degrees of freedom by mandating derivatives
of sufficient order at the first and last domain points be zero.

By leaving the derivatives argument NIL, natural polynomial splines
will be produced.  If you set derivatives to a list of degree-1
numerical values, these will be used as values for the remaining
derivatives.  The order is as follows:

For even degree:
First degree/2 values are first-point derivatives,
Last degree/2-1 values are last-point derivatives.

For odd degree:
First (degree-1)/2 are first-point,
Last (degree-1)/2 are last-point.

For ultimate flexibility, use the constraints argument as a list of
linear equation lists where all elements except the last are a row of
the coefficient matrix and the last element is the RHS vector value.
The utility function polynomial-spline-constraint assists in
generating these."
  (let* ((derivatives
          (coerce (aif derivatives
                       (->double-float it)
                       (loop for i below (1- degree)
                          collecting 0d0))
                  'vector))
         (npoints (length points))
         (N (1- npoints))
         (nrows (* (1+ degree) N))
         (coefs (gsl-spmatrix-alloc nrows
                                    nrows))
         (vec (gsl-vector-alloc (* (1+ degree) N)))
         (equation-index 0)
         (xs (coerce (cars points) 'vector))
         (ys (coerce (cdrs points) 'vector))
         (deltas (- (subseq xs 1) xs))
         (w (make-splinalg-workspace nrows nrows))
         (tol (->double-float tolerance))
         (res (gsl-vector-alloc nrows))
         (sp (make-polynomial-spline :coefs (make-array (list N (1+ degree)))
                                     :degree degree
                                     :xs xs
                                     :deltas deltas)))
    ;;; coefficients matrix
    ;;;
    ;;; Five constraint types:
    ;;; 1. Left boundaries.
    ;;; 2. Right boundaries.
    ;;; 3. Continuity.
    ;;; 4. Left natural derivatives.
    ;;; 5. Right natural derivatives.

    ;; 1. Left boundaries.
    (loop
       for i below N
       for j = (* (1+ degree) i)
       do
       ;; coefs
         (gsl-spmatrix-set coefs i j
                           1d0)
       ;; vector
         (gsl-vector-set vec i (->double-float (aref ys i))))
    (incf equation-index N)
    ;; 2. Right boundaries
    (loop
       for i below N
       for ii = (+ equation-index i)
       do
       ;; coefs
         (loop
            for j to degree
            for jj = (+ j (* (1+ degree) i))
            do (gsl-spmatrix-set coefs ii jj
                                 1d0))
       ;; vector
         (gsl-vector-set vec ii
                         (->double-float
                          (aref ys (1+ i)))))
    (incf equation-index N)
    ;; Continuity
    (loop
       for L from 1 below degree ; degree-1 fold
       do
         (loop
            for i below (1- N)
            for ii = (+ equation-index
                        (* (- L 1) (1- N))
                        i)
            do
            ;; coefs
            ;; rhs
              (gsl-spmatrix-set coefs ii (+ (* (1+ degree)
                                               (1+ i))
                                            L)
                                -1d0)
            ;; lhs
              (loop
                 for j from L to degree
                 for jj = (+ (* (1+ degree)
                                i)
                             j)
                 do (gsl-spmatrix-set coefs ii jj
                                      (->double-float
                                       (* (binomial j L)
                                          (expt (/ (aref deltas (1+ i))
                                                   (aref deltas i))
                                                L)))))
            ;; vec
              (gsl-vector-set vec ii 0d0)))
    (incf equation-index (* (1- degree) (1- N)))
    ;; either natural derivatives or explicit constraints:
    (if (and constraints
             (length-equal constraints (1- degree)))
        (loop
           for i from equation-index
           for constraint in constraints
           do
             (loop
                for j from 0
                for c in (butlast constraint)
                do (gsl-spmatrix-set coefs i j
                                     (->double-float c)))
             (gsl-vector-set vec i
                             (->double-float
                              (first (last constraint)))))
        ;; natural derivatives
        (cond
          ((= degree 2)
           (gsl-spmatrix-set coefs
                             equation-index
                             2
                             1d0)
           ;; vec
           (gsl-vector-set vec equation-index 0d0))
          (t
           (let* ((leftstart (if (evenp degree)
                                 (floor degree 2)
                                 (floor (+ degree 1) 2)))
                  (rightstart (if (evenp degree)
                                  (+ (floor degree 2) 1)
                                  (floor (+ degree 1) 2))))
             ;; 4. left
             (loop
                for i from 0
                for L from leftstart below degree
                for ii = (+ equation-index i)
                do
                ;; coefs
                  (gsl-spmatrix-set coefs
                                    ii
                                    L
                                    1d0)
                ;; vec
                  (gsl-vector-set vec ii (aref derivatives i)))
             (incf equation-index (- degree leftstart))
             ;; 5. right
             (loop
                for i from 0
                for L from rightstart below degree
                for ii = (+ equation-index i)
                do
                ;; coefs
                  (loop
                     for j from L to degree
                     for jj = (+ j
                                 (* (1+ degree)
                                    (- npoints 2)))
                     do
                       (gsl-spmatrix-set coefs ii jj
                                         (->double-float
                                          (npermutations j L))))
                ;; vec
                  (gsl-vector-set vec ii (aref derivatives (+ i (- degree leftstart)))))))))
    ;; solve
    (let* ((m (gsl-spmatrix-ccs coefs))
           (stat +GSL-CONTINUE+))
      (loop
         while (= stat +GSL-CONTINUE+)
         do (setf stat
                  (gsl-splinalg-itersolve-iterate
                   m vec
                   tol res
                   w)))
      (loop
         for i below nrows
         for ii = (floor i (1+ degree))
         for jj = (mod i (1+ degree))
         do (setf (aref (polynomial-spline-coefs sp) ii jj)
                  (gsl-vector-get res i)))
      ;; cleanup
      (gsl-spmatrix-free m)
      (gsl-spmatrix-free coefs)
      (gsl-vector-free vec)
      (gsl-vector-free res)
      (gsl-splinalg-itersolve-free w))
    (values (lambda (x)
              (evaluate-polynomial-spline sp x))
            sp)))

(defun gsl-spline (points
                   &key (type gsll:+cubic-spline-interpolation+))
  "Returns a Lisp function which returns the spline interpolation of
these points using GSLL.  Defaults to a cubic spline.

Returns 0 outside of the original domain since GSLL croaks outside of
it for at least the cubic spline."
  (let* ((points (sort (copy-list points)
                       #'<
                       :key #'car))
         (xmin (apply #'min (cars points)))
         (xmax (apply #'max (cars points)))
         (xs (grid:make-foreign-array 'double-float
                                      :initial-contents
                                      (cars points)))

         (ys (grid:make-foreign-array 'double-float
                                      :initial-contents
                                      (cdrs points)))
         (spline
          (gsll:make-spline type xs ys)))
    (values (lambda (x)
              (if (not (<= xmin x xmax))
                  0d0
                  (gsll:evaluate spline x)))
            spline)))
