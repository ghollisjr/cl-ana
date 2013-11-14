;;;; plotting.lisp

;;;; Interesting macro to use: with-output-to-string.  Use it to
;;;; create a string-stream object and write to it inside the body.
;;;; The resulting string is returned as the value.  This could be
;;;; extremely useful when generating the command sequences to send to
;;;; gnuplot.
