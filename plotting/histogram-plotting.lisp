(in-package :plotting)

(defclass histogram-plotter (line)
  ((histogram
    :initform nil
    :initarg :histogram
    :accessor histogram-plotter-hist
    :documentation "Histogram to be plotted.")))
