(in-package :file-utils)

(defun read-fields-from-file (file)
  (loop
     for line = (read-line file nil 'eof)
     until (equal line 'eof)
     collecting (line->fields line)))

(defun line->fields (line)
  (labels ((rec (stream acc)
             (handler-case (rec stream
                                (cons (read stream)
                                      acc))
               (error nil (nreverse acc)))))
    (with-input-from-string (s line)
      (rec s nil))))

(defun read-fields-from-pathname (pathname)
  (with-open-file (infile pathname
                          :direction :input
                          :if-does-not-exist :error)
    (read-fields-from-file infile)))
