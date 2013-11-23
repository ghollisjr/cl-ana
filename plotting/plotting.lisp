;;;; plotting.lisp

;;;; Interesting macro to use: with-output-to-string.  Use it to
;;;; create a string-stream object and write to it inside the body.
;;;; The resulting string is returned as the value.  This could be
;;;; extremely useful when generating the command sequences to send to
;;;; gnuplot.

(defclass titled ()
  ((title
    :initarg :title
    :initform ""
    :accessor title
    :documentation "Gnuplot likes to name everything with what it
    calls a title, so I've created a base class for this
    functionality.")))

;; A page is a whole window, sheet of paper, etc.  It can contain
;; multiple plots/graphs.
(defclass page (titled)
  ())

;; A plot is defined by an abscissa and an ordinate, though these
;; don't necessarily have to be drawn, as well as the margins and any
;; text written inside.  Each plot may contain one or more lines.
(defclass plot (titled)
  ((lines
    :initarg :lines
    :initform ()
    :accessor plot-lines
    :documentation "The lines which are part of the plot.")
   (legend
    :initarg :legend
    :initform nil
    :accessor plot-legend
    :documentation "The legend (if any) to be drawn on the plot.")))

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
;; own individual name/title.  These may be listed together in a key or
;; legend.
(defclass line (titled)
  ((data
    :initarg :data
    :initform ()
    :accessor line-data
    :documentation "The individual data points to be plotted; can be
    2-D or 3-D, in either case the line-data is an alist mapping the
    independent value (or values as a list) to the dependent value.")))

(defclass legend ()
  (()))
