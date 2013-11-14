;;;; gnuplot-i-cffi.lisp

;;;; This software uses the gnuplot_i library written by N. Devillard.

(in-package :gnuplot-i-cffi)

(define-foreign-library gnuplot-i-cffi
  (t (:default "libgnuploti")))

(use-foreign-library gnuplot-i-cffi)

;; To start a gnuplot session:
(defcfun "gnuplot_init" :pointer)

;; This function acts like printf, but I'm not sure I'll use these
;; capabilities.  It feeds a command string into the gnuplot session
;; you specify; this provides complete access to gnuplot.
(defcfun "gnuplot_cmd" :void
  (session-handle :pointer) ; as returned by gnuplot_init
  (command :string)
  &rest)

;; To close the gnuplot session (important for cleaning up temp files
;; in /tmp etc.):
(defcfun "gnuplot_close" :void
  (session-handle :pointer))
