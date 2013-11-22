;;;; plotting.lisp

;;;; Interesting macro to use: with-output-to-string.  Use it to
;;;; create a string-stream object and write to it inside the body.
;;;; The resulting string is returned as the value.  This could be
;;;; extremely useful when generating the command sequences to send to
;;;; gnuplot.

;; A page is like the TCanvas, a whole window, sheet of paper, etc.
(defclass page ()
  ())

;; A plot is defined by an abscissa and an ordinate, though these
;; don't necessarily have to be drawn, as well as the margins and any
;; text written inside.  Each plot may contain one or more lines.
(defclass plot ()
  ((lines
    :initarg :lines
    :initform ()
    :accessor plot-lines
    :documentation "The lines which are part of the plot.")))

;; A two-dimensional plot has up to four labelled axes:
;;
;; "x" for the axis along the bottom,
;; "y" for the axis along the left edge,
;; "x2" for the axis along the top, and
;; "y2" for the axis along the right edge.
(defclass plot2d (plot)
  ())

;; A three dimensional plot has up to three labelled axes, "x", "y",
;; and "z".
(defclass plot3d (plot)
  ())

;; A line is a single function or data set.  Each line may have its
;; own individual name.  These may be listed together in a key or
;; legend.
(defclass line ()
  ())
