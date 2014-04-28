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
;;;; gnuplot-i-cffi, which in turn uses gnuplot_i (see
;;;; gnuplot-i-cffi's files for appropriate copyright info).
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
;;;; There are a couple of convenience generic functions for plotting:
;;;; quick-draw and line.
;;;;
;;;; quick-draw is for quickly drawing some object in graphical form.
;;;;
;;;; line is for generating a line representing some object.
;;;;
;;;; quick-draw by the default method creates a 2-D plot using
;;;; line on the object, which means that in most cases all you
;;;; have to do is define a method of line for your type and you
;;;; can already use quick-draw.
;;;;
;;;; test.lisp demonstrates an example of using the full structure for
;;;; creating more complex plots; one can presumably define his own
;;;; functions for generating complex plots with certain conventions.
;;;; However I do intend to write a few convenience functions for
;;;; this, perhaps using plists to denote the structure of the
;;;; plotting objects.

(in-package :plotting)

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
(defclass page (titled)
  ((next-id
    :initform 0
    :documentation "The next available id for a page."
    :reader page-next-id
    :allocation :class)
   ;; I'll keep using the id
   (id
    ;;:initarg :id
    :initform -1
    :reader page-id
    :documentation "The numerical id of the page; necessary for
    gnuplot to know to create a distinct page rather than reuse the
    last one used.")
   (session
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
   (default-dimensions
       :initform (cons 800 600)
     :accessor page-default-dimensions
     :allocation :class
     :documentation "Default page size.")
   (dimensions
    :initform nil
    :initarg :dimensions
    :accessor page-dimensions
    :documentation "A cons pair (width . height) in pixels for the
    page size.")
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
    :initform "wxt"
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
  (with-accessors ((title title)
                   (shown-title page-shown-title)
                   (id page-id)
                   (terminal page-terminal)
                   (output page-output)
                   (layout page-layout)
                   (dimensions page-dimensions)
		   (default-dimensions page-default-dimensions)
                   (scale page-scale)
                   (plots page-plots))
      p
    (string-append
     (with-output-to-string (s)
       (format s "set output~%")
       (if (or (equal title "")
               (null title))
	   (format s "set term ~a ~a title 'Page ~a'" terminal id id)
	   (format s "set term ~a ~a title '~a'" terminal id title))
       (if dimensions
	   (format s " size ~a,~a" (car dimensions) (cdr dimensions))
	   (format s " size ~a,~a"
		   (car default-dimensions)
		   (cdr default-dimensions)))
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
     "unset multiplot")))

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
    (incf (slot-value p 'next-id))
    (setf session (spawn-gnuplot-session))
    (when (not layout)
      (setf layout (cons 1 (length plots))))))

(defmethod initialize-instance ((p page)
                                &key id
                                  &allow-other-keys)
  (let ((next-id (slot-value p 'next-id)))
    (when (not id)
      (setf (slot-value p 'id) next-id)))
  (call-next-method))

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

(defgeneric plot-cmd (plot)
  (:documentation "The command used for plotting the plot in gnuplot;
  can be plot, splot, etc."))

(defgeneric plot-axis-commands (plot)
  (:documentation "Returns a list of the commands to send to gnuplot
  which will set the proper axis labels"))

(defmethod generate-cmd ((p plot))
  (with-accessors ((title title)
                   (lines plot-lines)
                   (legend plot-legend))
      p
    (with-output-to-string (s)
      (format s "set title '~a'~%" title)
      (loop
         for a in (plot-axis-commands p)
         do (format s "~a~%" a))
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
                         (line-data-cmd l)))))))

(defgeneric plot-add-line (plot line)
  (:documentation "Adds a line to the plot; usually updates the legend
  based on the legend's update strategy.")
  (:method (plot line)
    (with-accessors ((lines plot-lines)
                     (legend plot-legend))
        plot
      (push line lines)
      (setf legend
            (legend-update legend line)))))

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

(defmethod plot-axis-commands ((p plot2d))
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

(defclass legend ()
  ((contents
    :initform ""
    :initarg :contents
    :accessor legend-contents
    :documentation "The legend contents to be shown.")
   (update-strategy
    :initform #'identity
    :initarg :update-strategy
    :accessor legend-update-strategy
    :documentation "A function which updates the legend based on the
    addition of a line.  Could be simply add the title to the legend,
    or do nothing.  Strategy is functional, i.e. it should return a
    lew legend based on the old one and the new line, not actually
    change the state of the current legend.")))

(defgeneric legend-update (legend line)
  (:documentation "Updates the legend for the plot based on the
  legend's update strategy.")
  (:method (legend line)
    (with-accessors ((strategy legend-update-strategy))
        legend
      (setf legend
            (funcall strategy legend line)))))

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

(defun lines (&rest line-arg-lists)
  "Function for applying line to a list of argument lists to line.

Example: (lines (list #'sin :sampling '(:low -3 :high 3 :nsamples 100)
                (list #'cos :sampling '(:low -3 :high 3 :nsamples 100))))"
  (mapcar (lambda (x) (apply #'line (mklist x)))
          line-arg-lists))

;; analytic functions:
(defmethod line ((s string) &key
                              (title "" title-given-p)
                              (style "lines")
                              line-type
                              line-width
                              color)
  (apply #'make-instance 'analytic-line
         :fn-string s
         :style style
         :color color
         :line-type line-type
         :line-width line-width
         (if title-given-p
             (list :title title)
             (list :title s))))

;; data:
(defmethod line ((data-alist list) &key
                                     (title "data")
                                     (style "points")
                                     point-type
                                     point-size
                                     line-type
                                     line-width
                                     color)
  "Assumes"
  (make-instance 'data-line
                 :title title
                 :data data-alist
                 :style style
                 :point-type point-type
                 :point-size point-size
                 :line-type line-type
                 :line-width line-width
                 :color color))

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
(defmethod line ((fn function) &key
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
                                 color)
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
    (line (sample-function fn
                           lower-bounds
                           upper-bounds
                           nsamples)
          :title title
          :style style
          :point-type point-type
          :point-size point-size
          :line-type line-type
          :line-width line-width
          :color color)))

;; histogram plotting:

;; still need to allow for error bars
(defmethod line ((histogram histogram)
                 &key
                   (title "histogram")
                   (style nil style-supplied-p)
                   fill-style
                   fill-density
                   color)
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
             (make-instance 'data-line
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
                            :color color))))
      (2
       (if (or (not (consp first-independent))
               (not (length-equal first-independent 2)))
           (error "Must be 2-d independent variable")
           (make-instance 'data-line
                          :title title
                          :data bin-data
                          :style (if style-supplied-p
                                     style
                                     "image")
                          :color color)))
      (otherwise (error "Can only plot 1-D or 2-D histograms")))))

;;; Terminal type functions:

(defun join-strings (&rest objects)
  (with-output-to-string (s)
    (loop
       for o in objects
       do (format s "~a" o))))

;; Function for generating the terminal type string of a page for
;; images in gnuplot
(defun png-term (&key
                   (size (cons 640 480))
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
                   font-size
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

(defun ps-term (&key
                  (size (cons 640 480))
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
             (list "ps"
                   (when size
                     (list 'size (car size) "," (cdr size)))
                   (when font-face
                     (list "font"
                           font-face
                           (when font-point-size
                             font-point-size)))
                   font-size
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

(defun eps-term (&rest args)
  "Generates term type string for eps terminals; takes the same
arguments as ps-term minus the orientation argument (this is used by
gnuplot to distinguish eps from ps)"
  (apply #'ps-term
         :orientation "eps"
         args))
