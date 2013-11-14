(in-package :gsl-cffi)

(defconstant +GSL-EOF+ 32
  "End-of-file return value for gsl functions")

(define-foreign-library gsl-cffi
  (t (:default "libgsl")))

(defcfun "gsl_ntuple_read" :int
  (ntuple :pointer)) ; gsl_ntuple*

;;; Currently unused:

(defcfun "gsl_multifit_fsolver_alloc" :pointer
  (gsl-multifit-fsolver-type :pointer) ; gsl_multifit_fsolver_type*
  (n-data :uint)
  (n-params :uint))

(defcfun "gsl_multifit_fsolver_free" :void
  (gsl-multifit-fsolver :pointer)) ; gsl_multifit_solver*

(defcfun "gsl_multifit_fsolver_set" :int
  (solver :pointer) ; gsl_multifit_fsolver*
  (gsl-multifit-function :pointer) ; gsl_multifit_function*
  (initial-guess :pointer)) ; const gsl_vector*
