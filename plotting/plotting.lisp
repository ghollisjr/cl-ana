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

(defvar *gnuplot-file-io* "/tmp/cl-ana.plotting/"
  "Set to a directory path to use files for data to be transferred to
gnuplot via files instead of pipes.  Value of NIL indicates pipe IO to
be used for data transfer, but this is potentially unsafe for large
transfers and can lead to hard to diagnose bugs.")

(defvar *gnuplot-file-io-index* 0)

(defvar *gnuplot-max-file-io-index* 0
  "Stores maximum file index")

;; Cleaning up on exit:
(defun plotting-exit-hook ()
  (when *gnuplot-file-io*
    (setf *gnuplot-max-file-io-index*
          (max *gnuplot-max-file-io-index*
               *gnuplot-file-io-index*))
    (let* ((plotdir (plotdir)))
      (when (probe-file plotdir)
        (loop
           for i from 1 to *gnuplot-max-file-io-index*
           do (delete-file
               (merge-pathnames (format nil
                                        "data~a.dat"
                                        i)
                                (make-pathname :directory
                                               (namestring plotdir)))))
        #+sbcl
        (sb-ext:delete-directory plotdir)
        #+clisp
        (ext:delete-dir (make-pathname :directory
                                       (namestring plotdir)))))))
(defparameter *plotting-exit-hook-p* nil)
(defun ensure-exit-hook ()
  (when (not *plotting-exit-hook-p*)
    #+sbcl
    (push #'plotting-exit-hook sb-ext:*exit-hooks*)
    #+clisp
    (push #'plotting-exit-hook custom:*fini-hooks*)
    (setf *plotting-exit-hook-p* t)))
(ensure-exit-hook)

(defgeneric line-file-io-p (object)
  (:documentation "Returns T if object uses files for safe input, or
  NIL if files are not needed.")
  (:method (obj)
    t))

(defun getpid ()
  #+sbcl
  (sb-posix:getpid)
  #+clisp
  (system::process-id)
  ;; Can't seem to find information on how to do this for other
  ;; implementations
  )

(defun plotdir ()
  (mkdirpath (->absolute-pathname
              (string-append
               (namestring *gnuplot-file-io*)
               "/"
               (mkstr (getpid))))))

(defun reset-data-path ()
  (when *gnuplot-file-io*
    (let ((dir (plotdir)))
      (setf *gnuplot-max-file-io-index*
            (max *gnuplot-max-file-io-index*
                 *gnuplot-file-io-index*))
      (loop
         for i from 1 to *gnuplot-file-io-index*
         do (let ((pn
                   (namestring
                    (merge-pathnames
                     (format nil "data~a.dat"
                             i)
                     dir))))
              (ensure-directories-exist pn)
              (when (probe-file pn)
                (delete-file pn))))
      (when (probe-file dir)
        #+sbcl
        (sb-ext:delete-directory dir))
      (setf *gnuplot-file-io-index* 0))))

(defun next-data-path ()
  (when *gnuplot-file-io*
    (let* ((dir (plotdir))
           (pn
            (namestring
             (merge-pathnames
              (format nil "data~a.dat"
                      (incf *gnuplot-file-io-index*))
              dir))))
      (ensure-directories-exist pn)
      pn)))

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

(defun ensure-gnuplot-session ()
  (when (not *gnuplot-sessions*)
    (spawn-gnuplot-session)))

(defun restart-gnuplot-sessions ()
  (loop
     for s in *gnuplot-sessions*
     do (gnuplot-close s))
  (when *gnuplot-file-io*
    (reset-data-path))
  (setf *gnuplot-sessions* nil)
  (spawn-gnuplot-session))

(defgeneric lisp->gnuplot (x)
  (:documentation "Handles any troublesome data types for sending to
  gnuplot and processes cons trees.")
  (:method (x)
    x)
  (:method ((x cons))
    (cons (lisp->gnuplot (car x))
          (lisp->gnuplot (cdr x))))
  (:method ((x double-float))
    (let* ((rawstr (mkstr x))
           (rawlen (length rawstr))
           (str
            ;; Remove unnecessary exponents
            (if (string= (subseq rawstr (- rawlen 2))
                         "d0")
                (subseq rawstr 0 (- rawlen 2))
                rawstr)))
      (map 'string
           (lambda (char)
             (if (or (char= char #\d)
                     (char= char #\f))
                 #\e
                 char))
           str))))

(defun gnuplot-format (stream &rest format-args)
  "Runs format with lisp->gnuplot mapped across all format arguments."
  (apply #'format stream
         (first format-args)
         (mapcar #'lisp->gnuplot (rest format-args))))

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
      (setf terminal (qt-term)))
    (when *gnuplot-file-io*
      (reset-data-path))
    (string-append
     (with-output-to-string (s)
       (gnuplot-format s "set output~%")
       (gnuplot-format s "set term ~a" terminal)
       (gnuplot-format s "~%")
       (when output (gnuplot-format s "set output '~a'~%" output))
       (gnuplot-format s "set multiplot layout ~a,~a title '~a'"
                       (car layout) (cdr layout)
                       (if shown-title
                           shown-title
                           ""))
       (when scale
         (gnuplot-format s "scale ~a,~a" (car scale) (cdr scale)))
       (gnuplot-format s "~%"))
     (reduce #'string-append
             (loop
                for plot in plots
                collecting (generate-cmd plot)))
     (with-output-to-string (s)
       (gnuplot-format s "unset multiplot~%")
       (gnuplot-format s "set output~%")
       ;; Return prompt
       (gnuplot-format s "print 'gnuplot> '")))
    ;; Should enable this later, but at the moment it causes an error
    ;;
    ;; ;; (when *gnuplot-file-io*
    ;; ;;   (reset-data-path))
    ))

;; Temporary solution to waiting on gnuplot to return prompt until
;; subprocess control library is added to cl-ana

(defun matches (string prompt)
  "Returns true if the end of the string is equal to prompt"
  (let ((string-length (length string))
        (prompt-length (length prompt)))
    (and (>= string-length
             prompt-length)
         (equal (subseq string
                        (- string-length
                           prompt-length))
                prompt))))

(defun dynamic-wait-fn (x min max delay slope
                        &optional (cutoff 50))
  "Generates a wait time based on the parameters.

* x should be >= 0,
* min and max control the minimum and maximum wait times,
* delay controls how long the minimum wait time should be used,
* slope controls how quickly the wait time transitions between the
  minimum and maximum wait times."
  (let ((raw (if (< x cutoff)
                 (atan (* slope (- x delay)))
                 (/ pi 2))))
    (+ (* (- max min)
          (/ pi)
          (+ (/ pi 2)
             raw))
       min)))

(defun read-stream-no-hang (stream)
  "Reads all output from stream without hanging at the end of output"
  (with-output-to-string (output)
    (do ((char (read-char-no-hang stream nil nil)
               (read-char-no-hang stream nil nil)))
        ((null char))
      (format output "~c" char))))

(defun clean-output (string)
  "Removes unnecessary control characters from string like ^M"
  (remove #\return string))

(defun receive (process)
  "Reads any available data from process output stream"
  (let ((stream (external-program:process-output-stream process)))
    (clean-output (read-stream-no-hang stream))))

(defun get-internal-real-time-in-seconds ()
  "Returns real internal time in seconds"
  (float (/ (get-internal-real-time)
            internal-time-units-per-second)))

(defun prompt-wait-by (process test
                       &key
                         (duration-min 0.05)
                         (duration-max 1)
                         (duration-delay 3)
                         (duration-slope 10.0)
                         max-wait-time)
  "Waits until session log passes test for at least duration amount of
time matches the prompt.  Returns value returned by test.

duration is the frequency at which to check for the prompt, and
consequently the amount of time required to establish prompt presence."
  (let* ((start-time (get-internal-real-time-in-seconds))
         (current-time start-time)
         (msg "")
         (return-val nil))
    (do ()
        ((setf return-val
               (funcall test msg))
         return-val)
      (setf current-time
            (get-internal-real-time-in-seconds))
      (when (and max-wait-time
                 (> (- current-time start-time)
                    max-wait-time))
        (error "Maximum wait time expired"))
      (sleep (dynamic-wait-fn (- current-time start-time)
                              duration-min
                              duration-max
                              duration-delay
                              duration-slope))
      (setf msg
            (string-append msg
                           (receive process))))))

(defun prompt-wait (process prompt
                    &key
                      (duration-min 0.05)
                      (duration-max 1)
                      (duration-delay 3)
                      (duration-slope 10.0)
                      max-wait-time)
  "Waits until the last message printed for at least duration amount
of time matches the prompt.

duration is the frequency at which to check for the prompt, and
consequently the amount of time required to establish prompt presence."
  (prompt-wait-by process (lambda (x)
                            (matches x prompt))
                  :duration-min duration-min
                  :duration-max duration-max
                  :duration-delay duration-delay
                  :duration-slope duration-slope
                  :max-wait-time max-wait-time))

(defgeneric draw (page &rest key-args)
  (:documentation "Draws the contents of a page using the multiplot
layout specified in the page.")
  (:method ((p page) &rest key-args)
    (with-accessors ((session page-gnuplot-session))
        p
      (gnuplot-cmd session (generate-cmd p))
      (prompt-wait session (format nil "gnuplot> ~%")))))

(defmethod initialize-instance :after
    ((p page) &key)
  (ensure-gnuplot-session)
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
   (title-offset
    :initarg :title-offset
    :initform nil
    :accessor plot-title-offset
    :documentation "A cons pair denoting the offset of the plot title")
   (legend
    :initarg :legend
    :initform (legend)
    :accessor plot-legend
    :documentation "The legend (if any) to be drawn on the plot.
    Either nil or a string, use the function legend to generate legend
    strings.")
   (grid
    :initarg :grid
    :initform (grid)
    :accessor plot-grid
    :documentation "The grid settings (if any) to be used in the plot.
    Must use output from grid function.")))

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
                   (title-offset plot-title-offset)
                   (lines plot-lines)
                   (labels plot-labels)
                   (legend plot-legend))
      p
    (let ((lines (remove-if #'null lines))
          (line-index->data-path
           (make-hash-table :test 'equal)))
      (with-output-to-string (s)
        (gnuplot-format s "set title '~a'" title)
        (when title-offset
          (gnuplot-format s " offset ~a,~a"
                          (car title-offset)
                          (cdr title-offset)))
        (gnuplot-format s "~%")
        (when legend
          (gnuplot-format s "~a~%" legend))
        (loop
           for a in (pre-plot-cmd-settings p)
           do (gnuplot-format s "~a~%" a))
        (gnuplot-format s "unset label~%")
        (loop
           for i from 1
           for label in labels
           do (gnuplot-format s "set label ~a ~a~%"
                              i label))
        (gnuplot-format s "~a " (plot-cmd p))
        (loop
           for line-index from 0
           for cons on lines
           do
             (progn
               (setf (gethash line-index
                              line-index->data-path)
                     (next-data-path))
               (let* ((raw-cmd (generate-cmd (car cons)))
                      (cmd
                       (if (and *gnuplot-file-io*
                                (line-file-io-p (car cons)))
                           (let ((subcmd
                                  (subseq raw-cmd 3)))
                             (format nil
                                     "'~a'~a"
                                     (gethash line-index
                                              line-index->data-path)
                                     subcmd))
                           raw-cmd)))
                 (gnuplot-format s "~a" cmd)))
           when (cdr cons)
           do (gnuplot-format s ", "))
        (gnuplot-format s "~%")
        ;; Send data either through pipes or file IO
        (if *gnuplot-file-io*
            ;; file IO
            (loop
               for line-index from 0
               for line in lines
               do (let ((pn (gethash line-index
                                     line-index->data-path)))
                    (with-open-file (file pn
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
                      (format file "~a~%"
                              (line-data-cmd line)))))

            ;; pipe IO
            (loop
               for l in lines
               do (gnuplot-format s "~a"
                                  (map 'string
                                       (lambda (c)
                                         (if (eq c #\f)
                                             #\e
                                             c))
                                       (line-data-cmd l)))))))))

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
    :documentation "List of axes which should be in log scale.  Valid
    axis names are \"x\", \"y\", and \"cb\".")
   (x-range
    :initarg :x-range
    :initform (cons "*" "*")
    :accessor plot2d-x-range
    :documentation "Sets the domain for the plot; a cons where the car
    is the lower bound and the cdr is the upper bound.")
   (x-tics
    :initarg :x-tics
    :initform nil
    :accessor plot2d-x-tics
    :documentation "Sets the x-axis tic options.  Can be a single
    string or a list of strings which will be added together.  Use
    tics function to generate string(s).")
   (x-mtics
    :initarg :x-mtics
    :initform nil
    :accessor plot2d-x-mtics
    :documentation "Controls the x-axis minor tic options.  Can be NIL
    for no minor tics, T for default minor tics, and an integer for
    the number of minor tics between major tics.")
   (x2-tics
    :initarg :x2-tics
    :initform nil
    :accessor plot2d-x2-tics
    :documentation "Sets the x2-axis tic options.  Can be a single
    string or a list of strings which will be added together.  Use
    tics function to generate string(s).")
   (x2-mtics
    :initarg :x2-mtics
    :initform nil
    :accessor plot2d-x2-mtics
    :documentation "Controls the x2-axis minor tic options.  Can be
    NIL for no minor tics, T for default minor tics, and an integer
    for the number of minor tics between major tics.")
   (y-range
    :initarg :y-range
    :initform (cons "*" "*")
    :accessor plot2d-y-range
    :documentation "Sets the range for the plot; a cons where the car
    is the lower bound and the cdr is the upper bound.")
   (y-tics
    :initarg :y-tics
    :initform nil
    :accessor plot2d-y-tics
    :documentation "y-axis tics.  See x-tics.")
   (y-mtics
    :initarg :y-mtics
    :initform nil
    :accessor plot2d-y-mtics
    :documentation "Controls the y-axis minor tic options.  Can be
    NIL for no minor tics, T for default minor tics, and an integer
    for the number of minor tics between major tics.")
   (y2-tics
    :initarg :y2-tics
    :initform nil
    :accessor plot2d-y2-tics
    :documentation "y2-axis tics.  See x-tics.")
   (y2-mtics
    :initarg :y2-mtics
    :initform nil
    :accessor plot2d-y2-mtics
    :documentation "Controls the y2-axis minor tic options.  Can be
    NIL for no minor tics, T for default minor tics, and an integer
    for the number of minor tics between major tics.")
   (cb-range
    :initarg :cb-range
    :initform (cons "*" "*")
    :accessor plot2d-cb-range
    :documentation "Sets the range for the colorbox (if applicable)
    for the plot; a cons where the car is the lower bound and the cdr
    is the upper bound.  A property of the plot since it is the z-axis
    for 2-d representations of 3-d objects.")
   (cb-tics
    :initarg :cb-tics
    :initform nil
    :accessor plot2d-cb-tics
    :documentation "color box tics.  See x-tics.")
   (cb-mtics
    :initarg :cb-mtics
    :initform nil
    :accessor plot2d-cb-mtics
    :documentation "Controls the cb-axis minor tic options.  Can be
    NIL for no minor tics, T for default minor tics, and an integer
    for the number of minor tics between major tics.")
   (x-title
    :initform nil
    :initarg :x-title
    :accessor plot2d-x-title
    :documentation "Title for bottom x axis")
   (x-title-offset
    :initform nil
    :initarg :x-title-offset
    :accessor plot2d-x-title-offset
    :documentation "Cons denoting offset for x-axis label")
   (x2-title
    :initform nil
    :initarg :x2-title
    :accessor plot2d-x2-title
    :documentation "Title for top x axis")
   (x2-title-offset
    :initform nil
    :initarg :x2-title-offset
    :accessor plot2d-x2-title-offset
    :documentation "Cons denoting offset for x2-axis label")
   (y-title
    :initform nil
    :initarg :y-title
    :accessor plot2d-y-title
    :documentation "Title for left y axis")
   (y-title-offset
    :initform nil
    :initarg :y-title-offset
    :accessor plot2d-y-title-offset
    :documentation "Cons denoting offset for y-axis label")
   (y2-title
    :initform nil
    :initarg :y2-title
    :accessor plot2d-y2-title
    :documentation "Title for right y axis")
   (y2-title-offset
    :initform nil
    :initarg :y2-title-offset
    :accessor plot2d-y2-title-offset
    :documentation "Cons denoting offset for y2-axis label")))

(defun plot2d (lines &rest key-args)
  "Creates a plot2d object with lines from lines argument and other
initargs from key-args."
  (apply #'make-instance 'plot2d :lines lines key-args))

(defmethod pre-plot-cmd-settings ((p plot2d))
  (flet ((maybe-make-str (label var &optional noquotes-p)
           (when var
             (list
              (if noquotes-p
                  (gnuplot-format nil "set ~a ~a~%" label var)
                  (gnuplot-format nil "set ~a '~a'~%" label var))))))
    (with-slots (logaxes
                 x-title x-title-offset
                 x2-title x2-title-offset
                 y-title y-title-offset
                 y2-title y2-title-offset)
        p
      (append
       (append
        (list "unset xlabel"
              "unset x2label"
              "unset x2tics"
              "unset ylabel"
              "unset y2label"
              "unset y2tics"
              "set nologscale x"
              "set nologscale y"
              "set nologscale zcb")
        ;; x
        (when x-title
          (list (string-append
                 (gnuplot-format nil "set xlabel '~a'"
                                 x-title)
                 (if x-title-offset
                     (gnuplot-format nil " offset ~a,~a"
                                     (car x-title-offset)
                                     (cdr x-title-offset)))
                 (gnuplot-format nil "~%"))))
        ;; x2
        (when x2-title
          (list (string-append
                 (gnuplot-format nil "set x2label '~a'"
                                 x2-title)
                 (if x2-title-offset
                     (gnuplot-format nil " offset ~a,~a"
                                     (car x2-title-offset)
                                     (cdr x2-title-offset)))
                 (gnuplot-format nil "~%"))))
        ;; y
        (when y-title
          (list (string-append
                 (gnuplot-format nil "set ylabel '~a'"
                                 y-title)
                 (if y-title-offset
                     (gnuplot-format nil " offset ~a,~a"
                                     (car y-title-offset)
                                     (cdr y-title-offset)))
                 (gnuplot-format nil "~%"))))
        ;; y2
        (when y2-title
          (list (string-append
                 (gnuplot-format nil "set y2label '~a'"
                                 y2-title)
                 (if y2-title-offset
                     (gnuplot-format nil " offset ~a,~a"
                                     (car y2-title-offset)
                                     (cdr y2-title-offset)))
                 (gnuplot-format nil "~%")))))
       (mapcan (lambda (axis)
                 (maybe-make-str "logscale"
                                 (if (equal axis "z")
                                     "zcb"
                                     axis)
                                 t))
               logaxes)))))

(defun grid
    (&key
       ;; Tics settings
       x-tics-p
       x-mtics-p
       x2-tics-p
       x2-mtics-p
       y-tics-p
       y-mtics-p
       y2-tics-p
       y2-mtics-p
       z-tics-p
       z-mtics-p
       cb-tics-p
       cb-mtics-p
       ;; Polar system angle
       polar
       ))

(defmethod plot-cmd ((p plot2d))
  (with-slots (x-range
               x-tics
               x-mtics
               x2-tics
               x2-mtics
               y-range
               y-tics
               y-mtics
               y2-tics
               y2-mtics
               cb-range
               cb-tics
               cb-mtics
               grid)
      p
    (with-output-to-string (s)
      ;; tics
      ;; Set defaults:
      (gnuplot-format s "~a" (merge-tics :x (tics :axis-p nil)))
      (gnuplot-format s "~a" (merge-tics :x2 nil))
      (gnuplot-format s "~a" (merge-tics :y (tics :axis-p nil)))
      (gnuplot-format s "~a" (merge-tics :y2 nil))
      (gnuplot-format s "~a" (merge-tics :cb (tics)))
      ;; Custom settings:
      (gnuplot-format s "~a" (merge-tics :x x-tics))
      (gnuplot-format s "~a" (merge-tics :x2 x2-tics))
      (gnuplot-format s "~a" (merge-tics :y y-tics))
      (gnuplot-format s "~a" (merge-tics :y2 y2-tics))
      (gnuplot-format s "~a" (merge-tics :cb cb-tics))
      ;; minor tics
      (gnuplot-format s "unset mxtics~%")
      (gnuplot-format s "unset mx2tics~%")
      (gnuplot-format s "unset mytics~%")
      (gnuplot-format s "unset my2tics~%")
      (gnuplot-format s "unset mcbtics~%")
      (when x-mtics
        (gnuplot-format s "set mxtics ~a~%"
                        (if (integerp x-mtics)
                            x-mtics
                            "default")))
      (when x2-mtics
        (gnuplot-format s "set mx2tics ~a~%"
                        (if (integerp x2-mtics)
                            x2-mtics
                            "default")))
      (when y-mtics
        (gnuplot-format s "set mytics ~a~%"
                        (if (integerp y-mtics)
                            y-mtics
                            "default")))
      (when y2-mtics
        (gnuplot-format s "set y2-mtics ~a~%"
                        (if (integerp y2-mtics)
                            y2-mtics
                            "default")))
      (when cb-mtics
        (gnuplot-format s "set mcbtics ~a~%"
                        (if (integerp cb-mtics)
                            cb-mtics
                            "default")))
      (if x-range
          (gnuplot-format s "set xrange [~a:~a]~%"
                          (car x-range) (cdr x-range))
          (gnuplot-format s "set xrange [*:*]~%"))
      (if y-range
          (gnuplot-format s "set yrange [~a:~a]~%"
                          (car y-range) (cdr y-range))
          (gnuplot-format s "set yrange [*:*]~%"))
      (if cb-range
          (gnuplot-format s "set cbrange [~a:~a]~%"
                          (car cb-range)
                          (cdr cb-range))
          (gnuplot-format s "set cbrange [*:*]~%"))

      ;; Grid settings
      ;; Enable this once grid is ready
      (when nil
        (gnuplot-format s "set grid ~a~%"
                        grid))

      ;; Plot command line:
      (gnuplot-format s "plot ")
      )))

;; A three dimensional plot has up to three labelled axes, "x", "y",
;; and "z".
(defclass plot3d (plot)
  ((logaxes
    :initarg :logaxes
    :initform nil
    :accessor plot3d-logaxes
    :documentation "List of axes which should be in log scale.  Valid
    axis names are \"x\", \"y\", and \"z\".")
   (view
       :initarg :view
       :initform nil
       :accessor plot3d-view
       :documentation "Sets the view for the 3-d plot.  Set to :map or
       \"map\" for contour plots.")
   (x-range
    :initarg :x-range
    :initform (cons "*" "*")
    :accessor plot3d-x-range
    :documentation "Sets the x-domain for the plot; a cons where the car
    is the lower bound and the cdr is the upper bound.")
   (x-tics
    :initarg :x-tics
    :initform nil
    :accessor plot3d-x-tics
    :documentation "Sets the x-axis tic options.  Can be a single
    string or a list of strings which will be added together.  Use
    tics function to generate string(s).")
   (x-mtics
    :initarg :x-mtics
    :initform nil
    :accessor plot2d-x-mtics
    :documentation "Controls the x-axis minor tic options.  Can be NIL
    for no minor tics, T for default minor tics, and an integer for
    the number of minor tics between major tics.")
   (y-range
    :initarg :y-range
    :initform (cons "*" "*")
    :accessor plot3d-y-range
    :documentation "Sets the y-domain for the plot; a cons where the
    car is the lower bound and the cdr is the upper bound.")
   (y-tics
    :initarg :y-tics
    :initform nil
    :accessor plot3d-y-tics
    :documentation "y-axis tics.  See x-tics.")
   (y-mtics
    :initarg :y-mtics
    :initform nil
    :accessor plot2d-y-mtics
    :documentation "Controls the y-axis minor tic options.  Can be NIL
    for no minor tics, T for default minor tics, and an integer for
    the number of minor tics between major tics.")
   (z-range
    :initarg :z-range
    :initform (cons "*" "*")
    :accessor plot3d-z-range
    :documentation "Sets the z-range for the plot; a cons where the
    car is the lower bound and the cdr is the upper bound.")
   (z-tics
    :initarg :z-tics
    :initform nil
    :accessor plot3d-z-tics
    :documentation "z-axis tics.  See x-tics.")
   (z-mtics
    :initarg :z-mtics
    :initform nil
    :accessor plot2d-z-mtics
    :documentation "Controls the z-axis minor tic options.  Can be NIL
    for no minor tics, T for default minor tics, and an integer for
    the number of minor tics between major tics.")
   (cb-range
    :initarg :cb-range
    :initform (cons "*" "*")
    :accessor plot3d-cb-range
    :documentation "color box tics.  See x-tics.")
   (cb-tics
    :initarg :cb-tics
    :initform nil
    :accessor plot2d-cb-tics
    :documentation "Sets the color box tic options.  Use tics function
    to generate string.")
   (cb-mtics
    :initarg :cb-mtics
    :initform nil
    :accessor plot2d-cb-mtics
    :documentation "Controls the cb-axis minor tic options.  Can be
    NIL for no minor tics, T for default minor tics, and an integer
    for the number of minor tics between major tics.")
   (x-title
    :initform nil
    :initarg :x-title
    :accessor plot3d-x-title
    :documentation "Title for first x axis")
   (y-title
    :initform nil
    :initarg :y-title
    :accessor plot3d-y-title
    :documentation "Title for first y axis")
   (z-title
    :initform nil
    :initarg :z-title
    :accessor plot3d-z-title
    :documentation "Title for first z axis")
   ;; 3d-specific
   (colorbox-p
    :initform nil
    :initarg :colorbox-p
    :accessor plot3d-colorbox-p
    :documentation "Boolean controlling whether colorbox is used")
   (pm3d
    :initform nil
    :initarg :pm3d
    :accessor plot3d-pm3d
    :documentation "pm3d settings string; use pm3d function to
    generate these strings")))

(defmethod pre-plot-cmd-settings ((p plot3d))
  (flet ((maybe-make-str (label var &optional noquotes-p)
           (when var
             (list
              (if noquotes-p
                  (gnuplot-format nil "set ~a ~a~%" label var)
                  (gnuplot-format nil "set ~a '~a'~%" label var))))))
    (with-slots (logaxes
                 x-title x2-title
                 y-title y2-title
                 z-title z2-title)
        p
      (append
       (append
        (list "unset view"
              "unset colorbox"
              "unset xlabel"
              "unset ylabel"
              "unset zlabel"
              "set nologscale x"
              "set nologscale y"
              "set nologscale z")
        (maybe-make-str "xlabel" x-title)
        (maybe-make-str "ylabel" y-title)
        (maybe-make-str "zlabel" z-title)
        (mapcan (lambda (axis)
                  (maybe-make-str "logscale"
                                  axis))
                logaxes))))))

(defmethod plot-cmd ((p plot3d))
  (with-slots (x-range
               x-tics
               x-mtics
               y-range
               y-tics
               y-mtics
               z-range
               z-tics
               z-mtics
               cb-range
               cb-tics
               cb-mtics
               grid
               colorbox-p
               pm3d
               view)
      p
    (with-output-to-string (s)
      ;; tics
      ;; Set defaults:
      (gnuplot-format s "~a" (merge-tics :x (tics :axis-p nil)))
      (gnuplot-format s "~a" (merge-tics :y (tics :axis-p nil)))
      (gnuplot-format s "~a" (merge-tics :z (tics :axis-p nil)))
      (gnuplot-format s "~a" (merge-tics :cb (tics :axis-p nil)))
      ;; Custom settings:
      (gnuplot-format s "~a" (merge-tics :x x-tics))
      (gnuplot-format s "~a" (merge-tics :y y-tics))
      (gnuplot-format s "~a" (merge-tics :z z-tics))
      (gnuplot-format s "~a" (merge-tics :cb cb-tics))
      ;; minor tics
      (gnuplot-format s "unset mxtics~%")
      (gnuplot-format s "unset mytics~%")
      (gnuplot-format s "unset mztics~%")
      (gnuplot-format s "unset mcbtics~%")
      (when x-mtics
        (gnuplot-format s "set mxtics ~a~%"
                        (if (integerp x-mtics)
                            x-mtics
                            "default")))
      (when y-mtics
        (gnuplot-format s "set mytics ~a~%"
                        (if (integerp y-mtics)
                            y-mtics
                            "default")))
      (when z-mtics
        (gnuplot-format s "set mztics ~a~%"
                        (if (integerp z-mtics)
                            z-mtics
                            "default")))
      (when cb-mtics
        (gnuplot-format s "set mcbtics ~a~%"
                        (if (integerp cb-mtics)
                            cb-mtics
                            "default")))

      ;; Grid settings
      ;; Enable this when grid is ready
      (when nil
        (gnuplot-format s "set grid ~a~%"
                        grid))

      ;; ranges
      (if x-range
          (gnuplot-format s "set xrange [~a:~a]~%"
                          (car x-range) (cdr x-range))
          (gnuplot-format s "set xrange [*:*]~%"))
      (if y-range
          (gnuplot-format s "set yrange [~a:~a]~%"
                          (car y-range) (cdr y-range))
          (gnuplot-format s "set yrange [*:*]~%"))
      (if z-range
          (gnuplot-format s "set zrange [~a:~a]~%"
                          (car z-range)
                          (cdr z-range))
          (gnuplot-format s "set zrange [*:*]~%"))
      (if cb-range
          (gnuplot-format s "set cbrange [~a:~a]~%"
                          (car cb-range)
                          (cdr cb-range))
          (gnuplot-format s "set cbrange [*:*]~%"))
      (when view
        (if (or (and (symbolp view)
                     (eq view :map))
                (and (stringp view)
                     (string= view "map")))
            (gnuplot-format s "set view map~%")
            (gnuplot-format s "set view ~a~%" view)))
      (when pm3d
        (gnuplot-format s "set ~a~%" pm3d))
      (when colorbox-p
        (gnuplot-format s "set colorbox~%"))
      ;; Plot command line:
      (gnuplot-format s "splot "))))

(defun plot3d (lines &rest key-args)
  "Returns a plot3d object supplied lines and key-args"
  (apply #'make-instance 'plot3d :lines lines
         key-args))

;; pm3d
(defun pm3d (&key
               ;; at should be a string of up to six of the characters
               ;; b, s, and t.  See gnuplot documentation for usage.
               at
               ;; interpolate can be T or a cons (m . n) for
               ;; interpolation between points.
               interpolate
               ;; scan can be :automatic, :forward, :backward or
               ;; :depthorder
               (scan :automatic)
               ;; flush can be :begin, :center or :end
               (flush :begin)
               (ftriangles-p t)
               ;; clip-strict controls clip1in or clip4in setting
               clip-strict-p
               ;; corners2color can be :mean, :geomean, :harmean,
               ;; :rms, :median, :min, :max, :c1, :c2, :c3, :c4
               (corners2color :mean)
               ;; hidden 3d can be T or a linestyle option
               hidden3d
               (explicit-p t)
               ;; map option is deprecated and unnecessary, so it's
               ;; omitted.
               )
  (with-output-to-string (s)
    (gnuplot-format s "pm3d")
    (when at
      (gnuplot-format s " at ~a" at))
    (when interpolate
      (gnuplot-format s " interpolate")
      (when (consp interpolate)
        (gnuplot-format s " ~a,~a"
                        (car interpolate)
                        (cdr interpolate))))
    (case scan
      (:automatic (gnuplot-format s " scansautomatic"))
      (:forward (gnuplot-format s " scansforward"))
      (:backward (gnuplot-format s " scansbackward"))
      (:depthorder (gnuplot-format s " depthorder")))
    (gnuplot-format s " flush ~a"
                    (string-downcase
                     (string flush)))
    (gnuplot-format s " ~a"
                    (if ftriangles-p
                        "ftriangles"
                        "noftriangles"))
    (if clip-strict-p
        (gnuplot-format s " clip4in")
        (gnuplot-format s " clip1in"))
    (gnuplot-format s " corners2color ~a"
                    (string-downcase
                     (string corners2color)))
    (if hidden3d
        (if (eq hidden3d t)
            (gnuplot-format s " hidden3d")
            (gnuplot-format s " hidden3d ~a" hidden3d))
        (gnuplot-format s " nohidden3d"))
    (if explicit-p
        (gnuplot-format s " explicit")
        (gnuplot-format s " implicit"))))

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
    (gnuplot-format s "\"~a\" " text)
    (when position
      (gnuplot-format s "at ")
      (when coordinate-system
        (gnuplot-format s "~a " coordinate-system))
      (gnuplot-format s "~{~a~^,~} "
                      position))
    (gnuplot-format s "~a "
                    (case justification
                      (:left "left")
                      (:right "right")
                      (:center "center")))
    (when rotation
      (gnuplot-format s "rotate by ~a "
                      ;; needs to be double for gnuplot to understand
                      (->double-float rotation)))
    (when font
      (gnuplot-format s "\"~a" font)
      (when font-size
        (gnuplot-format s ",~a"
                        (->double-float font-size)))
      (gnuplot-format s "\" "))
    (when (not enhanced-p)
      (gnuplot-format s "noenhanced "))
    (gnuplot-format s "~a "
                    (case layer
                      (:front "front")
                      (:back "back")))
    (when color
      (gnuplot-format s "tc ~s "
                      color))
    (if point-style
        (gnuplot-format s "point ~a "
                        point-style)
        (gnuplot-format s "nopoint "))
    (when offset
      (gnuplot-format s "offset ~{~a~^,~}"
                      offset))))

;; Tics
(defun merge-tics (axis tics)
  "Generates tics command(s) from tics.  axis should be :x, :y, :z,
or :cb"
  (let ((axis
         (case axis
           (:x "xtics")
           (:x2 "x2tics")
           (:y "ytics")
           (:y2 "y2tics")
           (:z "ztics")
           (:z2 "z2tics")
           (:cb "cbtics"))))
    (cond
      ((null tics)
       "")
      ((listp tics)
       (with-output-to-string (s)
         (loop
            for tic in tics
            for i from 0
            do (if (zerop i)
                   (gnuplot-format s "set ~a ~a~%"
                                   axis tic)
                   (gnuplot-format s "set ~a add ~a~%"
                                   axis tic)))))
      (t
       (gnuplot-format nil "set ~a ~a~%"
                       axis tics)))))

(defun tics (&key
               axis-p ; tics on axis instead of axis
               mirror-p ; put tics on opposite axis as well
               (location :in) ; can be :in or :out
               ;; scale options:
               major-scale
               minor-scale
               rotate ; rotation angle in degrees
               offset ; offset
               ;;; Note: Only one of sampling or manual-labels should
               ;;; be used at a time.  To add manual-labels to
               ;;; automatic tics, supply separate tic strings to the
               ;;; plot in a list.

               ;; sampling can be
               ;;
               ;; 1. A number for the increment
               ;; 2. A list containing the start, increment and
               ;;    optionally end
               ;; 3. NIL, denoting default behavior
               sampling
               ;; manual-labels is a list of label specification
               ;; plists.  Each plist has the following slots:
               ;;
               ;; * :name is the text for the label
               ;; * :position is the position for the label
               ;; * :level is an optional slot and can be :major or
               ;;   :minor
               manual-labels
               ;; font settings
               font-face
               font-size
               color)
  (with-output-to-string (s)
    (gnuplot-format s "~a"
                    (if axis-p
                        "axis"
                        "border"))
    (gnuplot-format s " ~a"
                    (if mirror-p
                        "mirror"
                        "nomirror"))
    (gnuplot-format s " ~a"
                    (string-downcase (string location)))
    (gnuplot-format s " scale ")
    (if major-scale
        (progn
          (gnuplot-format s "~a" major-scale)
          (when minor-scale
            (gnuplot-format s ", ~a"
                            minor-scale)))
        (gnuplot-format s "default"))
    (if rotate
        (gnuplot-format s " rotate ~a" rotate)
        (gnuplot-format s " norotate"))
    (if offset
        (gnuplot-format s " offset ~a" offset)
        (gnuplot-format s " nooffset"))
    (cond
      ((null sampling)
       (when (not manual-labels)
         (gnuplot-format s " autofreq")))
      ((listp sampling)
       (destructuring-bind (start incr &optional end)
           sampling
         (gnuplot-format s " ~a, ~a" start incr)
         (when end
           (gnuplot-format s ", ~a" end))))
      (t
       (gnuplot-format s " ~a" sampling)))
    (when manual-labels
      (gnuplot-format s " (")
      (loop
         for ml in manual-labels
         for nleft downfrom (1- (length manual-labels))
         do (destructuring-bind (&key name position level)
                ml
              (when name
                (gnuplot-format s "~s " name))
              (gnuplot-format s "~a" position)
              (when level
                (let ((level
                       (case level
                         (:major 0)
                         (:minor 1))))
                  (gnuplot-format s " ~a" level)))
              (when (not (zerop nleft))
                (gnuplot-format s ","))))
      (gnuplot-format s ")"))
    (when font-face
      (gnuplot-format s " font \"~a"
                      font-face)
      (when font-size
        (gnuplot-format s ",~a"
                        font-size))
      (gnuplot-format s "\""))
    (when color
      (gnuplot-format s " textcolor ~a"
                      color))))

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
      (gnuplot-format s "~a with " plot-arg)
      (gnuplot-format s "~a " style)
      (when point-type
        (gnuplot-format s "pointtype ~a " point-type))
      (when point-size
        (gnuplot-format s "pointsize ~a " point-size))
      (when line-style
        (gnuplot-format s "linestyle ~a " line-style))
      (when line-type
        (gnuplot-format s "linetype ~a " line-type))
      (when line-width
        (gnuplot-format s "linewidth ~a " line-width))
      (when fill-style
        (gnuplot-format s "fillstyle ~a " fill-style)
	(when (and (equal fill-style "solid")
		   fill-density)
	  (gnuplot-format s "~a " fill-density)))
      (when color
        (gnuplot-format s "linecolor rgb '~a' " color))
      (gnuplot-format s "title '~a'" title)
      (when options
        (gnuplot-format s "~a " options)))))

(defclass data-line (line)
  ((data
    :initarg :data
    :initform ()
    :accessor data-line-data
    :documentation "The individual data points to be plotted; can be
    2-D or 3-D, in either case the line-data is an alist mapping the
    independent value (or values as a list) to the dependent value.")
   (pm3d-ncols
    :initarg :pm3d-ncols
    :initform nil
    :accessor data-line-pm3d-ncols
    :documentation "Controls whether the data should be formatted
    according to pm3d formatting or standard formatting.  If NIL,
    standard formatting is used.  If a numerical value, controls the
    number of columns in the data.")))

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
    (with-accessors ((data data-line-data)
                     (pm3d-ncols data-line-pm3d-ncols))
        line
      (let ((extractor
             (if data
                 (if pm3d-ncols
                     (let ((ncols pm3d-ncols)
                           (index 0))
                       (lambda (cons)
                         (gnuplot-format s "~{~a ~}"
                                         (car cons))
                         (gnuplot-format s "~a~%"
                                         (cdr cons))
                         (incf index)
                         (when (= index ncols)
                           (gnuplot-format s "~%")
                           (setf index 0))))
                     (if (listp (car (first data)))
                         (lambda (cons)
                           (gnuplot-format s "~{~a ~}"
                                           (car cons))
                           (gnuplot-format s "~a~%"
                                           (cdr cons)))
                         (lambda (cons)
                           (gnuplot-format s "~a ~a~%"
                                           (car cons)
                                           (cdr cons)))))
                 (error "Empty data in data-line"))))
        (loop
           for d in data
           do (funcall extractor d))
        (gnuplot-format s "e~%")))))

(defclass analytic-line (line)
  ((fn-string
    :initarg :fn-string
    :initform "0"
    :accessor analytic-line-fn-string
    :documentation "The function expression to plot, should be a
    function of x/x and y.")))

(defmethod line-file-io-p ((line analytic-line))
  nil)

(defun analytic-line-set-plot-arg (line)
  (with-slots (x-range y-range fn-string)
      line
    (setf
     (slot-value line 'plot-arg)
     (with-output-to-string (s)
       (gnuplot-format s "~a " fn-string)))))

(defmethod initialize-instance :after ((l analytic-line) &key)
  (analytic-line-set-plot-arg l))

(defmethod (setf line-plot-arg) :after (value (l analytic-line))
  (analytic-line-set-plot-arg l))

(defun legend (&key
                 ;; if non-nil, default legend settings
                 (default nil)
                 ;; if nil, don't show legend at all
                 (show t)
                 ;; If front-p is non-nil, legend will be drawn in
                 ;; front of data
                 front-p
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
      (gnuplot-format s "set key")
      (if show
          ;; show legend
          (progn
            (if front-p
                (gnuplot-format s " opaque")
                (gnuplot-format s " noopaque"))
            (gnuplot-format s " ~a"
                            (string-downcase (mkstr mode)))
            (when location
              (if (symbolp (car location))
                  (gnuplot-format s " ~a ~a"
                                  (string-downcase
                                   (mkstr (car location)))
                                  (string-downcase
                                   (mkstr (cdr location))))
                  (gnuplot-format s " at ~a,~a"
                                  (car location)
                                  (cdr location))))
            (gnuplot-format s " ~a"
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
              (gnuplot-format s " samplen ~a"
                              samplen))
            (when spacing
              (gnuplot-format s " spacing ~a"
                              spacing))
            (when width-inc
              (gnuplot-format s " width ~a"
                              width-inc))
            (when height-inc
              (gnuplot-format s " height ~a"
                              height-inc))
            (when title
              (gnuplot-format s " title \"~a\"" title))
            (if box
                (progn
                  (gnuplot-format s " box")
                  (when line-type
                    (gnuplot-format s " linetype ~a" line-type))
                  (when line-width
                    (gnuplot-format s " linewidth ~a" line-width))
                  (when line-style
                    (gnuplot-format s " linestyle ~a" line-style)))
                (gnuplot-format s " nobox")))
          ;; don't show legend
          (gnuplot-format s " off")))))

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
                   pm3d-ncols
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
           :pm3d-ncols pm3d-ncols
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

;;; Variable binning histograms
;; special use function for generating pm3d data from
;; variable-binning-histograms
(defun vhist->pm3d (vhist)
  (let* ((data (cl-ana.histogram::variable-binning-histogram-content
                vhist))
         (dim-specs
          (cl-ana.histogram::variable-binning-histogram-dim-specs
           vhist))
         (empty-value
          (hist-empty-bin-value vhist))
         (sample-points
          (apply #'cartesian-product
                 dim-specs)))
    (flet ((href (p)
             (cons p
                   (aif (gethash p data)
                        it
                        empty-value))))
      (mapcar #'href sample-points))))

(defmethod line ((hist variable-binning-histogram)
                 &rest key-args)
  (cond
    ;; 1-D
    ((= (length (hdn hist)) 1)
     (apply #'line (map->alist hist) key-args))
    ;; 2-D
    ((= (length (hdn hist)) 2)
     (apply #'line
            (vhist->pm3d hist)
            :style "pm3d"
            :pm3d-ncols
            (length
             (first
              (cl-ana.histogram::variable-binning-histogram-dim-specs
               hist)))
            key-args))
    ;; error
    (t (error "Only 1-D or 2-D histograms can be plotted"))))


;;; Terminal type functions:

(defun join-strings (&rest objects)
  (with-output-to-string (str)
    (loop
       for o in objects
       do (gnuplot-format str "~a" o))))

(defvar *window-number* 0)

(defun wxt-term (&key
                   (size (cons 800 600))
                   window-number
                   title)
  (when (not window-number)
    (setf window-number *window-number*)
    (incf *window-number*))
  (when (not title)
    (setf title
          (gnuplot-format nil "Page ~a" window-number)))
  (with-output-to-string (s)
    (gnuplot-format s "wxt")
    (gnuplot-format s " ~a" window-number)
    (gnuplot-format s " title '~a'" title)
    (when size
      (gnuplot-format s " size ~a,~a" (car size) (cdr size)))))

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
                       (gnuplot-format s "font \"~a" font-face)
                       (if font-size
                           (gnuplot-format s ",~a\"" font-size)
                           (gnuplot-format s "\""))))
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

;; epslatex terminal
(defun epslatex-term (&key
                        standalone-p
                        oldstyle-p
                        level1-p
                        (color-p t)
                        (size (cons "5" "3"))
                        (font-face "phv")
                        font-size
                        line-width
                        (dashed-p nil dashed-supplied-p)
                        dash-length
                        (rounded t)
                        palfuncparam-samples
                        palfuncparam-maxdeviation
                        ;; To supply noheader, give nil header
                        ;; argument
                        (header nil header-supplied-p)
                        blacktext-p)
  "Generates the type string for a epslatex terminal with options"
  (apply #'join-strings
         (intersperse
          " "
          (remove-if-not
           #'identity
           (alexandria:flatten
            (list "epslatex"
                  (if standalone-p
                      "standalone"
                      "input")
                  (if oldstyle-p
                      "oldstyle"
                      "newstyle")
                  (if level1-p
                      "level1"
                      "leveldefault")
                  (if color-p
                      "color"
                      "monochrome")
                  (when size
                    (list "size" (car size) "," (cdr size)))
                  (when font-face
                    (with-output-to-string (s)
                      (gnuplot-format s "font \"~a" font-face)
                      (if font-size
                          (gnuplot-format s ",~a\"" font-size)
                          (gnuplot-format s "\""))))
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
                  (when palfuncparam-samples
                    (if palfuncparam-maxdeviation
                        (gnuplot-format nil
                                        "palfuncparam ~a,~a"
                                        palfuncparam-samples
                                        palfuncparam-maxdeviation)
                        (gnuplot-format nil
                                        "palfuncparam ~a"
                                        palfuncparam-samples)))
                  (when header-supplied-p
                    (if header
                        (gnuplot-format nil "header ~s"
                                        header)
                        (gnuplot-format nil "noheader")))
                  (if blacktext-p
                      "blacktext"
                      "colortext")))))))

(defun qt-term (&key
                  window-number
                  (size (cons 640 480))
                  enhanced-p
                  font
                  title
                  persist-p)
  "Generates the type string for a qt terminal with options"
  (when (not window-number)
    (setf window-number *window-number*)
    (incf *window-number*))
  (when (not title)
    (setf title
          (gnuplot-format nil "Page ~a" window-number)))
  (with-output-to-string (s)
    (gnuplot-format s "~{~a ~}"
                    (remove nil
                            (list "qt"
                                  (when window-number
                                    (gnuplot-format nil "~a" window-number))
                                  (gnuplot-format nil "size ~a, ~a" (car size) (cdr size))
                                  (when enhanced-p
                                    "enhanced")
                                  (when font
                                    (gnuplot-format nil "font \"~a\"" font))
                                  (when title
                                    (gnuplot-format nil "title \"~a\"" title))
                                  (when persist-p
                                    "persist"))))))

;; Special function for outputting PDF plots since it requires precise
;; coordination between various functions
(defun draw-pdf (page output-prefix &rest terminal-keywords)
  "Generates a properly formatted PDF graph from gnuplot using LaTeX
formatting.  terminal-keywords are supplied to the epslatex-term used
in generating the plot.  Header keyword argument needs to enable math,
so if the user does not supply a :header argument then the default is
to enable math and use the Helvetica font."
  (let* ((debug-p nil)
         (output
          (string-append output-prefix ".tex"))
         (terminal
          (apply #'epslatex-term
                 :standalone-p t

                 (append
                  (when (not (find :header terminal-keywords))
                    (list
                     ;; Old method doesn't have capital greek symbols
                     ;; :header "\\usepackage{helvet}\\usepackage{sansmath}\\sansmath"
                     :header "\\usepackage{helvet}\\usepackage[italic]{mathastext}"
                     ))
                  terminal-keywords)))
         (current-dir
          #+sbcl
           (namestring (sb-posix:getcwd))
           #+clisp
           (ext:cd))
         (destdir
          (namestring
           (make-pathname :directory
                          (pathname-directory output)))))
    (setf (page-output page)
          output)
    (setf (page-terminal page)
          terminal)
    (draw page)
    #+sbcl
    (sb-posix:chdir destdir)
    #+clisp
    (ext:cd destdir)
    (external-program:run "pdflatex"
                          (list "--shell-escape"
                                output))
    (flet ((maybe-delete-file (x)
             (when (not debug-p)
               (handler-case (delete-file x)
                 (error () (format t "Warning: Error deleting ~a"
                                   x))))))
      (maybe-delete-file (string-append output-prefix ".aux"))
      (maybe-delete-file (string-append output-prefix "-inc.eps"))
      (maybe-delete-file (string-append output-prefix "-inc-eps-converted-to.pdf"))
      (maybe-delete-file (string-append output-prefix ".log"))
      (maybe-delete-file (string-append output-prefix ".tex")))
    #+sbcl
    (sb-posix:chdir current-dir)
    #+clisp
    (ext:cd current-dir))
  nil)

;; Plot merging functions:
(defun plotjoin! (&rest plots)
  "Function which joins the line lists from each plot.  Modifies the
first plot given, so make sure to create a fresh first plot.  Easiest
to do by create a fresh page and providing this to pagemerge.  Easily
creates comparison plots from individual plots."
  (let* ((result (first plots))
         (line-lists
          (mapcar (lambda (plot)
                    (copy-list (plot-lines plot)))
                  plots)))
    (setf (plot-lines result)
          (apply #'append line-lists))
    result))

(defun pagemerge! (&rest pages)
  "Function which maps plotjoin! across the plots in each page,
joining the nth plots together from each page via plotjoin!.  Modifies
the first page given, so make sure to create a fresh first page.
Easily creates comparison plots from individual pages.  Conceptually
it's preserving the structure of pages but combining the lines from
each plot together onto the same x-y or x-y-z axes.

Note that this function assumes that all pages contain the same number
of plots, so if this is not true, you should fill the missing plots in
your pages with plots containing NIL line lists,
e.g. (page (list (plot2d nil))) or (page (list (plot3d nil)))."
  (let* ((result (first pages))
         (plot-lists
          (mapcar (lambda (page)
                    (copy-list (page-plots page)))
                  pages)))
    (setf (page-plots result)
          (apply #'mapcar
                 (lambda (&rest plots)
                   (apply #'plotjoin! plots))
                 plot-lists))
    result))

(defun pagejoin! (layout &rest pages)
  "Function which combines pages by joining the plot lists together.
Must supply layout, and the first page is modified so make sure to
supply a fresh page as the first argument for safety."
  (let* ((result (first pages))
         (plot-lists (mapcar (lambda (page)
                               (copy-list (page-plots page)))
                             pages)))
    (setf (page-plots result)
          (apply #'append
                 plot-lists))
    (setf (page-layout result)
          layout)
    result))
