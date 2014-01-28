(in-package :gnuplot-interface)

(defun gnuplot-init ()
  (start "gnuplot" ()
         :input :stream))

(defun gnuplot-close (session)
  (close (process-input-stream session)))

(defun gnuplot-cmd (session command-string)
  (let ((input-stream (process-input-stream session)))
    (format input-stream
            "~a~%"
            command-string)
    (finish-output input-stream)))
