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

;;;; This library provides access to gnuplot from CL via
;;;; gnuplot-i-cffi, which in turn uses external-program.
;;;;
;;;; The structure of the classes follows gnuplot, with the exception
;;;; of the plot/graph distinction being seen as unnecessary.
;;;;
;;;; The structure is as follows:
;;;;
;;;; Page: The most basic thing which can be plotted upon.  Pages
;;;; contain some number of plots which are drawn on the page.
;;;;
;;;; Plot: A plot is either 2-D or 3-D, and defines the appropriate
;;;; axis.  A plot contains some number of lines which are drawn
;;;; together in the plot.
;;;;
;;;; Line: A line is a single function or collection of data which can
;;;; be plotted inside of a plot.
;;;;
;;;; The preferred approach to drawing is to use the draw, page,
;;;; plot2d/plot3d, and line functions to construct a plottable object
;;;; and then draw it.
;;;;
;;;; draw is the universal drawing function, it can plot a page, a
;;;; plot, or a line.  Additionally, since e.g. drawing a line
;;;; requires a page and plot to place it in, draw accepts arguments
;;;; for whatever plot objects must be generated as well.  (This does
;;;; mean there are multiple ways to use draw to draw the same thing,
;;;; but the freedom is nice).
;;;;
;;;; line is special in that it generates a line object from objects
;;;; of other types, e.g. (line #'sin) creates a line from sampling
;;;; #'sin and defaults the style to "lines".
;;;;
;;;; test.lisp demonstrates an example of using the full structure for
;;;; creating more complex plots; one can presumably define his own
;;;; functions for generating complex plots with certain conventions.
;;;; However I do intend to write a few convenience functions for
;;;; this, perhaps using plists to denote the structure of the
;;;; plotting objects.

(in-package :cl-ana.plotting)

;;;; I'm using a new strategy with gnuplot sessions: Each page gets
;;;; its own session; this way all windows stay interactive.  If the
;;;; memory footprint gets too big I'll have to do something about it,
;;;; but a suggestion to a similar concern was to use multiple
;;;; sessions so I'll see what happens.

(defvar *gnuplot-sessions* nil)

(defparameter *gnuplot-single-session* t
  "Set this parameter to nil if you want each page to have its own
  gnuplot session; this is expensive and requires
  (restart-gnuplot-sessions) occasionally for freeing memory.")

;; run any initialization functions:
(defun gnuplot-settings (session)
  ;; Image style settings:
  ;;(gnuplot-cmd session "set palette rgb 33,13,10")
  (gnuplot-cmd session "set palette defined (0 \"white\", 0 \"dark-violet\", 3 \"blue\", 8 \"light-green\", 13 \"orange\", 15 \"red\")")
  ;; Boxes style settings:
  (gnuplot-cmd session "set style fill solid 0.5")
  session)

(defun spawn-gnuplot-session ()
  (if *gnuplot-single-session*
      (progn
        (when (null *gnuplot-sessions*)
          (push (gnuplot-settings (gnuplot-init))
                *gnuplot-sessions*))
        (first *gnuplot-sessions*))
      (let ((session (gnuplot-settings
                      (gnuplot-init))))
        (gnuplot-settings session)
        (push session *gnuplot-sessions*)
        session)))

(spawn-gnuplot-session)

(defun restart-gnuplot-sessions ()
  (loop
     for s in *gnuplot-sessions*
     do (gnuplot-close s))
  (setf *gnuplot-sessions* nil)
  (spawn-gnuplot-session))

(defgeneric generate-cmd (object)
  (:documentation "Generates a command string (with new-line at end)
  for drawing a particular element.  Assumes that the appropriate
  context has been set up by the page/draw function."))

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
(defclass page ()
  ((session
    :initarg :gnuplot-session
    :initform nil
    :accessor page-gnuplot-session
    :documentation "The gnuplot session in use by the page.")
   ;;; actually set-able slots:
   (shown-title
    :initform nil
    :initarg :shown-title
    :accessor page-shown-title
    :documentation "Since I implement the plotting with multiplot in
    gnuplot, there is the possibility of the page having a title as
    well as the plots having a collective title.  This sets the shown
    title.")
   (scale
    :initform (cons 1 1)
    :initarg :scale
    :accessor page-scale
    :documentation "A cons pair (x-scale . y-scale) denoting the scale
    for each plot.")
   (plots
    :initarg :plots
    :initform ()
    :accessor page-plots
    :documentation "The list of plots which are currently part of the
    page.")
   (layout
    :initarg :layout
    :initform nil
    :accessor page-layout
    :documentation "A cons (numrows . numcols) telling how to arrange
    the plots in the multiplot.")
   (terminal
    :initarg :terminal
    :initform nil
    :accessor page-terminal
    :documentation "The type of page, gnuplot only supports a fixed
    number of types so this makes more sense to be added as a slot
    then to have different page types.")
   (output
    :initarg :output
    :initform nil
    :accessor page-output
    :documentation "Name for output file when appropriate")))

(defun page (plots &rest key-args)
  "Creates a page object with list of plots from plots argument and
other initargs from key-args."
  (apply #'make-instance 'page :plots plots key-args))

(defgeneric page-add-plot (page plot)
  (:documentation "Adds a plot to the page.")
  (:method (page plot)
    (push plot (page-plots page))))

(defmethod generate-cmd ((p page))
  (with-accessors ((shown-title page-shown-title)
                   (terminal page-terminal)
                   (output page-output)
                   (layout page-layout)
                   (scale page-scale)
                   (plots page-plots))
      p
    (when (not terminal)
      (setf terminal (wxt-term)))
    (string-append
     (with-output-to-string (s)
       (format s "set output~%")
       (format s "set term ~a" terminal)
       (format s "~%")
       (when output (format s "set output '~a'~%" output))
       (format s "set multiplot layout ~a,~a title '~a'"
               (car layout) (cdr layout)
               (if shown-title
                   shown-title
                   ""))
       (when scale
         (format s "scale ~a,~a" (car scale) (cdr scale)))
       (format s "~%"))
     (reduce #'string-append
             (loop
                for plot in plots
                collecting (generate-cmd plot)))
     (with-output-to-string (s)
       (format s "unset multiplot~%")
       (format s "set output")))))

(defgeneric draw (page &rest key-args)
  (:documentation "Draws the contents of a page using the multiplot
layout specified in the page.")
  (:method ((p page) &rest key-args)
    (with-accessors ((session page-gnuplot-session))
        p
      (gnuplot-cmd session (generate-cmd p)))))

(defmethod initialize-instance :after
    ((p page) &key)
  (with-slots (session layout plots)
      p
    (setf session (spawn-gnuplot-session))
    (when (not layout)
      (setf layout (cons 1 (length plots))))))

;; A plot is defined by an abscissa and an ordinate, though these
;; don't necessarily have to be drawn, as well as the margins and any
;; text written inside.  Each plot may contain one or more lines.
(defclass plot (titled)
  ((lines
    :initarg :lines
    :initform ()
    :accessor plot-lines
    :documentation "The lines which are part of the plot.")
   ;; thanks for the formatting emacs
   (labels
       :initarg :labels
       :initform ()
       :accessor plot-labels
       :documentation "List of labels to be drawn on the plot.  Use
       the label function to generate the command strings for each
       label.")
   (legend
    :initarg :legend
    :initform (legend)
    :accessor plot-legend
    :documentation "The legend (if any) to be drawn on the plot.
    Either nil or a string, use the function legend to generate legend
    strings.")))

(defgeneric plot-cmd (plot)
  (:documentation "The command used for plotting the plot in gnuplot;
  can be plot, splot, etc."))

(defgeneric pre-plot-cmd-settings (plot)
  (:documentation "Returns a list of the commands to send to gnuplot
  which will control settings needed prior to the drawing command.
  This includes setting the proper axis labels as well as possibly
  controlling the view in 3-D plots, etc."))

(defmethod generate-cmd ((p plot))
  (with-accessors ((title title)
                   (lines plot-lines)
                   (labels plot-labels)
                   (legend plot-legend))
      p
    (let ((lines (remove-if #'null lines)))
      (with-output-to-string (s)
        (format s "set title '~a'~%" title)
        (when legend
          (format s "~a~%" legend))
        (loop
           for a in (pre-plot-cmd-settings p)
           do (format s "~a~%" a))
        (loop
           for i from 1
           for label in labels
           do (format s "set label ~a ~a~%"
                      i label))
        (format s "~a " (plot-cmd p))
        (loop
           for cons on lines
           do (format s "~a" (generate-cmd (car cons)))
           when (cdr cons)
           do (format s ", "))
        (format s "~%")
        (loop
           for l in lines
           do (format s "~a"
                      (map 'string
                           (lambda (c)
                             (if (eq c #\f)
                                 #\e
                                 c))
                           (line-data-cmd l))))))))

;; A two-dimensional plot has up to four labelled axes:
;;
;; "x" for the axis along the bottom,
;; "y" for the axis along the left edge,
;; "x2" for the axis along the top, and
;; "y2" for the axis along the right edge.
(defclass plot2d (plot)
  ((logaxes
    :initarg :logaxes
    :initform nil
    :accessor plot2d-logaxes
    :documentation "List of axes which should be in log scale.")
   (x-range
    :initarg :x-range
    :initform (cons "*" "*")
    :accessor plot2d-x-range
    :documentation "Sets the domain for the plot; a cons where the car
    is the lower bound and the cdr is the upper bound.")
   (y-range
    :initarg :y-range
    :initform (cons "*" "*")
    :accessor plot2d-y-range
    :documentation "Sets the range for the plot; a cons where the car
    is the lower bound and the cdr is the upper bound.")
   (cb-range
    :initarg :cb-range
    :initform (cons "*" "*")
    :accessor plot2d-cb-range
    :documentation "Sets the range for the colorbox (if applicable)
    for the plot; a cons where the car is the lower bound and the cdr
    is the upper bound.  A property of the plot since it is the z-axis
    for 2-d representations of 3-d objects.")
   (x-title
    :initform nil
    :initarg :x-title
    :accessor plot2d-x-title
    :documentation "Title for bottom x axis")
   (x2-title
    :initform nil
    :initarg :x2-title
    :accessor plot2d-x2-title
    :documentation "Title for top x axis")
   (y-title
    :initform nil
    :initarg :y-title
    :accessor plot2d-y-title
    :documentation "Title for left y axis")
   (y2-title
    :initform nil
    :initarg :y2-title
    :accessor plot2d-y2-title
    :documentation "Title for right y axis")))

(defun plot2d (lines &rest key-args)
  "Creates a plot2d object with lines from lines argument and other
initargs from key-args."
  (apply #'make-instance 'plot2d :lines lines key-args))

(defmethod pre-plot-cmd-settings ((p plot2d))
  (flet ((maybe-make-str (label var &optional noquotes-p)
           (when var
             (list
              (if noquotes-p
                  (format nil "set ~a ~a~%" label var)
                  (format nil "set ~a '~a'~%" label var))))))
    (with-slots (logaxes x-title x2-title y-title y2-title)
        p
      (append
       (append
        (list "unset xlabel"
              "unset x2label"
              "unset ylabel"
              "unset y2label"
              "set nologscale x"
              "set nologscale y"
              "set nologscale zcb")
        (maybe-make-str "xlabel" x-title)
        (maybe-make-str "x2label" x2-title)
        (maybe-make-str "ylabel" y-title)
        (maybe-make-str "y2label" y2-title))
       (mapcan (lambda (axis)
                 (maybe-make-str "logscale"
                                 (if (equal axis "z")
                                     "zcb"
                                     axis)
                                 t))
               logaxes)))))

(defmethod plot-cmd ((p plot2d))
  (with-slots (x-range y-range cb-range)
      p
    (with-output-to-string (s)
      (if cb-range
          (format s "set cbrange [~a:~a]~%"
                  (car cb-range)
                  (cdr cb-range))
          (format s "set cbrange [*:*]~%"))
      (format s "plot ")
      (if x-range
          (format s "[~a:~a] "
                  (car x-range) (cdr x-range))
          (format s "[*:*] "))
      (when y-range
        (format s "[~a:~a] "
                (car y-range) (cdr y-range))
        (format s "[*:*] ")))))

;; A three dimensional plot has up to three labelled axes, "x", "y",
;; and "z".
(defclass plot3d (plot)
  ())

(defmethod plot-cmd ((p plot3d))
  "splot")

;; Labels
(defun label (text
              &key
                position
                coordinate-system ; coordinate system for position,
                                        ; see gnuplot manual
                (justification :center) ; :left, :center, or :right
                rotation ; nil for no rotation, or angle in degrees
                font
                font-size ; must set font as well if you want this to
                                        ; take affect
                (enhanced-p t) ; set to nil if you don't want enhanced
                                        ; text
                (layer :front) ; can be :front or :back, :front means
                                        ; text is shown on top of graph lines,
                                        ; :back means graph is shown on top of
                                        ; text
                color
                point-style ; set to a point style if you want a point to be
                                        ; plotted around the label (controlled by offset)
                offset) ; set to a a list (dx dy dz) to displace the
                                        ; point from the text
  (with-output-to-string (s)
    (format s "\"~a\" " text)
    (when position
      (format s "at ")
      (when coordinate-system
        (format s "~a " coordinate-system))
      (format s "~{~a~^,~} "
              position))
    (format s "~a "
            (case justification
              (:left "left")
              (:right "right")
              (:center "center")))
    (when rotation
      (format s "rotate by ~a "
              ;; needs to be double for gnuplot to understand
              (->double-float rotation)))
    (when font
      (format s "\"~a" font)
      (when font-size
        (format s ",~a" (->double-float font-size)))
      (format s "\" "))
    (when (not enhanced-p)
      (format s "noenhanced "))
    (format s "~a "
            (case layer
              (:front "front")
              (:back "back")))
    (when color
      (format s "tc ~s "
              color))
    (if point-style
        (format s "point ~a "
                point-style)
        (format s "nopoint "))
    (when offset
      (format s "offest ~{~a~^,~}"
              offset))))

;; A line is a single function or data set.  Each line may have its
;; own individual name/title.  These may be listed together in a key or
;; legend.
(defclass line (titled)
  ((style
    :initform "lines"
    :initarg :style
    :accessor line-style
    :documentation "Plotting style.  Defaults to lines for ease of
    implemenation, users should change this if they want another
    style, like points.")
   (color
    :initform nil
    :initarg :color
    :accessor line-color
    :documentation "Color for plotting the line.")
   (point-type
    :initform nil
    :initarg :point-type
    :accessor line-point-type
    :documentation "Type of point to draw when using style points or
    linespoints.")
   (point-size
    :initform nil
    :initarg :point-size
    :accessor line-point-size
    :documentation "Size of points when appropriate.")
   (line-style
    :initform nil
    :initarg :line-style
    :accessor line-line-style
    :documentation "linestyle, can refer to custom-defined linestyles.
    Linestyles should be defined prior to reference, so you should
    send the gnuplot session appropriate commands to define them prior
    to plotting.")
   (line-type
    :initform nil
    :initarg :line-type
    :accessor line-line-type
    :documentation "Line type")
   (line-width
    :initform nil
    :initarg :line-width
    :accessor line-line-width
    :documentation "Thickness of line when appropriate.")
   (fill-style
    :initform nil
    :initarg :fill-style
    :accessor line-fill-style
    :documentation "The fill style for boxes: either solid or empty")
   (fill-density
    :initform nil
    :initarg :fill-density
    :accessor line-fill-density
    :documentation "The amount of coloration to fill when using boxes
    with fill style solid; must be a floating point number between 0
    and 1.")
   (plot-arg
    :initarg :plot-arg
    :initform ""
    :accessor line-plot-arg
    :documentation "The string denoting the plot argument which should
    directly proceed the plot/splot command; can be a function body,
    '-', or a file name.")
   (options
    :initarg :options
    :initform ""
    :accessor line-options
    :documentation "Miscellaneous options not covered by the standard
    ones above; this is to permit things like image plotting.")))

(defmethod line-data-cmd ((l line))
  "")

(defmethod generate-cmd ((l line))
  (with-output-to-string (s)
    (with-accessors ((title title)
                     (style line-style)
                     (line-style line-line-style)
                     (point-type line-point-type)
                     (point-size line-point-size)
                     (line-type line-line-type)
                     (line-width line-line-width)
                     (fill-style line-fill-style)
                     (fill-density line-fill-density)
                     (plot-arg line-plot-arg)
                     (color line-color)
                     (options line-options))
        l
      (format s "~a with " plot-arg)
      (format s "~a " style)
      (when point-type
        (format s "pointtype ~a " point-type))
      (when point-size
        (format s "pointsize ~a " point-size))
      (when line-style
        (format s "linestyle ~a " line-style))
      (when line-type
        (format s "linetype ~a " line-type))
      (when line-width
        (format s "linewidth ~a " line-width))
      (when fill-style
        (format s "fillstyle ~a " fill-style)
	(when (and (equal fill-style "solid")
		   fill-density)
	  (format s "~a " fill-density)))
      (when color
        (format s "linecolor rgb '~a' " color))
      (format s "title '~a'" title)
      (when options
        (format s "~a " options)))))

(defclass data-line (line)
  ((data
    :initarg :data
    :initform ()
    :accessor data-line-data
    :documentation "The individual data points to be plotted; can be
    2-D or 3-D, in either case the line-data is an alist mapping the
    independent value (or values as a list) to the dependent value.")))

(defmethod initialize-instance :after ((l data-line) &key)
  (with-slots (data plot-arg style)
      l
    (setf plot-arg "'-'")
    (let ((first-dependent (cdr (first data))))
      (when (subtypep (type-of first-dependent) 'err-num)
        (setf data
              (mapcar
               (lambda (x)
                 (let ((e (cdr x)))
                   (cons (cons (car x)
                               (list (err-num-value e)))
                         (err-num-error e))))
               data))
        (setf style "yerrorbars")))))

(defmethod line-data-cmd ((line data-line))
  (with-output-to-string (s)
    (with-accessors ((data data-line-data))
        line
      (let ((extractor
             (if data
                 (if (listp (car (first data)))
                     (lambda (cons)
                       (format s "~{~a ~}" (car cons))
                       (format s "~a~%" (cdr cons)))
                     (lambda (cons)
                       (format s "~a ~a~%"
                               (car cons)
                               (cdr cons))))
                 (error "Empty data in data-line"))))
        (loop
           for d in data
           do (funcall extractor d))
        (format s "e~%")))))

(defclass analytic-line (line)
  ((fn-string
    :initarg :fn-string
    :initform "0"
    :accessor analytic-line-fn-string
    :documentation "The function expression to plot, should be a
    function of x/x and y.")))

(defun analytic-line-set-plot-arg (line)
  (with-slots (x-range y-range fn-string)
      line
    (setf
     (slot-value line 'plot-arg)
     (with-output-to-string (s)
       (format s "~a " fn-string)))))

(defmethod initialize-instance :after ((l analytic-line) &key)
  (analytic-line-set-plot-arg l))

(defmethod (setf line-plot-arg) :after (value (l analytic-line))
  (analytic-line-set-plot-arg l))

(defun legend (&key
                 ;; if non-nil, default legend settings
                 (default nil)
                 ;; if nil, don't show legend at all
                 (show t)
                 ;; can be drawn :outside or :inside the plotting area
                 (mode :inside)
                 ;; tell legend where to draw itself.  A cons pair of
                 ;; either coordinates or a combination of (:left
                 ;; :center :right) and (:top :center :bottom)
                 ;; (e.g. (cons :left :top), (cons 5 20))
                 (location (cons :right :top))
                 ;; can be :vertical, :horizontal, :left or :right alligned
                 (arrangement :vertical)
                 ;; length of sample line
                 samplen
                 ;; spacing between entries
                 spacing
                 ;; legend width increment (see gnuplot manual)
                 width-inc
                 ;; legend height increment (see gnuplot manual)
                 height-inc
                 ;; autotitle is of no use due to design of interface
                 ;; explicit title of legend
                 title
                 ;; box borders drawn (non-nil) or not (nil)
                 box
                 ;; box border line type and width
                 line-type
                 line-width
                 ;; or can use user-defined style:
                 line-style)
  (when (not default)
    (with-output-to-string (s)
      (format s "set key")
      (if show
          ;; show legend
          (progn
            (format s " ~a"
                    (string-downcase (mkstr mode)))
            (when location
              (if (symbolp (car location))
                  (format s " ~a ~a"
                          (string-downcase
                           (mkstr (car location)))
                          (string-downcase
                           (mkstr (cdr location))))
                  (format s " at ~a,~a"
                          (car location)
                          (cdr location))))
            (format s " ~a"
                    (case arrangement
                      (:vertical
                       "vertical")
                      (:horizontal
                       "horizontal")
                      (:left
                       "Left")
                      (:right
                       "Right")))
            (when samplen
              (format s " samplen ~a"
                      samplen))
            (when spacing
              (format s " spacing ~a"
                      spacing))
            (when width-inc
              (format s " width ~a"
                      width-inc))
            (when height-inc
              (format s " height ~a"
                      height-inc))
            (when title
              (format s " title \"~a\"" title))
            (if box
                (progn
                  (format s " box")
                  (when line-type
                    (format s " linetype ~a" line-type))
                  (when line-width
                    (format s " linewidth ~a" line-width))
                  (when line-style
                    (format s " linestyle ~a" line-style)))
                (format s " nobox")))
          ;; don't show legend
          (format s " off")))))

;;; Convenience functions:

;;; Methods on draw for plots, lines, etc.
(defmethod draw ((line line) &rest key-args)
  "Method on draw for a line.  Takes as keyword arguments plot-args
and page-args, which themselves are plists for the keyword arguments
appropriate for plots and pages respectively."
  ;; Default method for 2-D plotting
  (destructuring-bind (&key
                       plot-args
                       page-args)
      key-args
    (draw
     (apply #'page
            (list
             (apply #'plot2d
                    (list
                     line)
                    plot-args))
            page-args))))

(defmethod draw ((plot2d plot2d) &rest key-args)
  "Method on draw for a plot2d object.  key-args should be a plist
denoting the page initargs."
  (draw
   (apply #'page
          (list
           plot2d)
          key-args)))

(defmethod draw (object &rest key-args)
  (destructuring-bind (&key
                       page-args
                       plot-args
                       line-args)
      key-args
    (draw
     (apply #'page
            (list
             (apply #'plot2d
                    (list (apply #'line
                                 object
                                 line-args))
                    plot-args))
            page-args))))

;;; line construction:
(defgeneric line (object &key &allow-other-keys)
  (:documentation "Returns a line appropriate for plotting object.")
  ;; so lines are handled automatically
  (:method ((l line) &key)
    l))

;; analytic functions:
(defmethod line ((s string) &rest other-keys
                 &key
                   (title "" title-given-p)
                   (style "lines")
                   line-type
                   line-width
                   color
                   &allow-other-keys)
  (apply #'make-instance 'analytic-line
         :fn-string s
         :style style
         :color color
         :line-type line-type
         :line-width line-width
         :allow-other-keys t
         (append (if title-given-p
                     (list :title title)
                     (list :title s))
                 other-keys)))

;; data:
(defmethod line ((data-alist list) &rest other-keys
                 &key
                   (title "data")
                   (style "points")
                   point-type
                   point-size
                   line-type
                   line-width
                   color
                   &allow-other-keys)
  "Assumes"
  (when data-alist
    (apply #'make-instance 'data-line
           :title title
           :data data-alist
           :style style
           :point-type point-type
           :point-size point-size
           :line-type line-type
           :line-width line-width
           :color color
           :allow-other-keys t
           other-keys)))

(defun sample-function (fn lower-bounds upper-bounds nsamples)
  "Samples a function which takes a single argument according to
lower-bounds, upper-bounds and nsamples.  Each boundary must be either
a numerical value or a list of numerical values interpreted as
vectors; therefore they all must be of the same type (i.e. list or
atom).

Returns an alist mapping each independent value to the value of fn at
that point."
  (let* ((atomic (atom lower-bounds))
         (lower-bounds-list
          (mapcar #'->double-float
                  (mklist lower-bounds)))
         (upper-bounds-list
          (mapcar #'->double-float
                  (mklist upper-bounds)))
         (samples-list
          (mapcar #'->double-float
                  (mklist nsamples)))
         ;; taking advantage of the tensor functions:
         (deltas (/ (- upper-bounds-list lower-bounds-list)
                    (- samples-list 1)))
         (raw-domain
          (apply #'cartesian-product
                 (mapcar #'range
                         lower-bounds-list
                         upper-bounds-list
                         deltas)))
         (domain (if atomic
                     (first (transpose raw-domain))
                     raw-domain)))
    (zip domain
         (mapcar fn domain))))

;; functions:
(defmethod line ((fn function) &rest other-keys
                 &key
                   (sampling
                    (list :low -3d0
                          :high 3d0
                          :nsamples 100))
                   (title "function")
                   (style "lines")
                   point-type
                   point-size
                   line-type
                   line-width
                   color
                   &allow-other-keys)
  "Samples your function based on the keyword arguments and creates a
  data-line mapping your function to the output values.

Conventions are:

fn must evaluate to a float (single or double).

All sampling arguments must be of the same type, and must be either
atoms or lists.

If the sampling arguments are atoms, then fn is assumed to take a
single double-float argument.

If the sampling arguments are lists, then fn is assumed to take a list
of up to two double-float arguments."
  (let ((lower-bounds
         (let ((low (getf sampling :low)))
           (if low
               low
               -3d0)))
        (upper-bounds
         (let ((high (getf sampling :high)))
           (if high
               high
               3d0)))
        (nsamples
         (let ((nsamples (getf sampling :nsamples)))
           (if nsamples
               nsamples
               100))))
    (apply #'line (sample-function fn
                                   lower-bounds
                                   upper-bounds
                                   nsamples)
           :title title
           :style style
           :point-type point-type
           :point-size point-size
           :line-type line-type
           :line-width line-width
           :color color
           other-keys)))

;; histogram plotting:

;; still need to allow for error bars
(defmethod line ((histogram rectangular-histogram)
                 &rest other-keys
                 &key
                   (title "")
                   (style nil style-supplied-p)
                   fill-style
                   fill-density
                   color
                   &allow-other-keys)
  (let* ((hist (sparse->contiguous histogram))
         (ndims (hist-ndims hist))
         (bin-data
          (mapcar (lambda (datum-cons)
                    (let ((bin-center (car datum-cons))
                          (bin-value (cdr datum-cons)))
                      (cons (if (listp bin-center)
                                (mapcar (alexandria:rcurry #'float 0d0)
                                        bin-center)
                                (float bin-center 0d0))
                            (float bin-value 0d0))))
                  (map->alist hist)))
         (first-independent (car (first bin-data))))
    (case ndims
      (1
       (if (not (atom first-independent))
           (error "Must be 1-d independent variable")
           (let ((first-dependent (cdr (first bin-data))))
             (apply #'make-instance 'data-line
                    :title title
                    :data bin-data
                    :fill-style fill-style
                    :fill-density fill-density
                    :style
                    (if style-supplied-p
                        style
                        (if (subtypep (type-of first-dependent)
                                      'err-num)
                            "boxerrorbars"
                            "boxes"))
                    :color color
                    :allow-other-keys t
                    other-keys))))
      (2
       (if (or (not (consp first-independent))
               (not (length-equal first-independent 2)))
           (error "Must be 2-d independent variable")
           (apply #'make-instance 'data-line
                  :title title
                  :data bin-data
                  :style (if style-supplied-p
                             style
                             "image")
                  :color color
                  :allow-other-keys t
                  other-keys)))
      (otherwise (error "Can only plot 1-D or 2-D histograms")))))

;; (defmethod line ((hist variable-binning-histogram)
;;                  &key
;;                    sampling
;;                    (title "")
;;                    (style nil style-supplied-p)
;;                    fill-style
;;                    fill-density
;;                    color
;;                    &allow-other-keys)
;;   (when (not sampling)
;;     (error "Must supply sampling information for variable-binning-histogram"))
;;   (case (hist-ndims hist)
;;     (1
;;      (let ((centers
;;             (mapcar (lambda (dim-spec)
;;                       (let ((result nil))
;;                         (do* ((ds dim-spec (rest ds))
;;                               (c (/ (+ (first ds) (second ds))
;;                                     2)
;;                                  (/ (+ (first ds) (second ds))
;;                                     2)))
;;                              ((null (cdr dim-spec)) (nreverse result))
;;                           (push c result))))
;;                     dim-specs))

;;; Terminal type functions:

(defun join-strings (&rest objects)
  (with-output-to-string (s)
    (loop
       for o in objects
       do (format s "~a" o))))

(defvar *wxt-id* 0)

(defun wxt-term (&key
                   (size (cons 800 600))
                   id
                   title)
  (when (not id)
    (setf id *wxt-id*)
    (incf *wxt-id*))
  (when (not title)
    (setf title
          (with-output-to-string (s)
            (format s "Page ~a" id))))
  (with-output-to-string (s)
    (format s "wxt")
    (format s " ~a" id)
    (format s " title '~a'" title)
    (when size
      (format s " size ~a,~a" (car size) (cdr size)))))

;; Function for generating the terminal type string of a page for
;; images in gnuplot
(defun png-term (&key
                   (size (cons 800 600))
                   (font-face "arial")
                   font-point-size
                   ;; fontsize can be :tiny, :small, :medium, :large, :giant
                   (font-size :medium)
                   transparent
                   interlace
                   truecolor
                   (rounded t)
                   enhanced
                   colors)
  "Generates the type string for a png terminal with options"
  (string-downcase
   (apply #'join-strings
          (intersperse
           " "
           (remove-if-not
            #'identity
            (alexandria:flatten
             (list "png"
                   (when size
                     (list 'size (car size) "," (cdr size)))
                   (when font-face
                     (list "font"
                           font-face
                           (when font-point-size
                             font-point-size)))
                   (when (not font-point-size)
                     font-size)
                   (when transparent
                     "transparent")
                   (when interlace
                     "interlace")
                   (when truecolor
                     "truecolor")
                   (when (not rounded)
                     "butt")
                   (when enhanced
                     "enhanced")
                   (when colors
                     colors))))))))

;; JPEG terminal
(defun jpeg-term (&key
                    (size (cons 800 600))
                    (font-face "arial")
                    font-point-size
                    ;; fontsize can be :tiny, :small, :medium, :large, :giant
                    (font-size :medium)
                    interlace
                    enhanced
                    colors)
  "Generates the type string for a png terminal with options"
  (string-downcase
   (apply #'join-strings
          (intersperse
           " "
           (remove-if-not
            #'identity
            (alexandria:flatten
             (list "jpeg"
                   (when size
                     (list 'size (car size) "," (cdr size)))
                   (when font-face
                     (list "font"
                           font-face
                           (when font-point-size
                             font-point-size)))
                   (when (not font-point-size)
                     font-size)
                   (when interlace
                     "interlace")
                   (when enhanced
                     "enhanced")
                   (when colors
                     colors))))))))

;; Does not correctly generate PS file
(defun ps-term (&key
                  (size (cons 800 600))
                  (font-face "arial")
                  font-point-size
                  ;; fontsize can be :tiny, :small, :medium, :large, :giant
                  (font-size :medium)
                  transparent
                  interlace
                  truecolor
                  (rounded t)
                  enhanced
                  colors)
  "Generates the type string for a postscript terminal with options"
  (string-downcase
   (apply #'join-strings
          (intersperse
           " "
           (remove-if-not
            #'identity
            (alexandria:flatten
             (list "postscript"
                   (when size
                     (list 'size (car size) "," (cdr size)))
                   (when font-face
                     (list "font"
                           font-face
                           (when font-point-size
                             font-point-size)))
                   (when (not font-point-size)
                     font-size)
                   (when transparent
                     "transparent")
                   (when interlace
                     "interlace")
                   (when truecolor
                     "truecolor")
                   (when (not rounded)
                     "butt")
                   (when enhanced
                     "enhanced")
                   (when colors
                     colors))))))))

;; Does not produce correct EPS file
(defun eps-term (&rest args)
  "Generates term type string for eps terminals; takes the same
arguments as ps-term minus the orientation argument (this is used by
gnuplot to distinguish eps from ps)"
  (apply #'ps-term
         :orientation "eps"
         args))

(defun pdf-term (&key
                   (color-p t)
                   (size (cons "5" "3"))
                   (font-face "arial")
                   font-size
                   line-width
                   (dashed-p nil dashed-supplied-p)
                   dash-length
                   (rounded t)
                   enhanced)
  "Generates the type string for a png terminal with options"
  (string-downcase
   (apply #'join-strings
          (intersperse
           " "
           (remove-if-not
            #'identity
            (alexandria:flatten
             (list "pdf"
                   (if color-p
                       "color"
                       "monochrome")
                   (when size
                     (list 'size (car size) "," (cdr size)))
                   (when font-face
                     (with-output-to-string (s)
                       (format s "font \"~a" font-face)
                       (if font-size
                           (format s ",~a\"" font-size)
                           (format s "\""))))
                   (when line-width
                     (list "linewidth" line-width))
                   (if (not dashed-supplied-p)
                       (if color-p
                           "solid"
                           "dashed")
                       (if dashed-p
                           "dashed"
                           "solid"))
                   (when dashed-p
                     (when dash-length
                       (list "dl" dash-length)))
                   (when (not rounded)
                     "butt")
                   (when enhanced
                     "enhanced"))))))))
