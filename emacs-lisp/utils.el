(defmacro in-slime-buffer (&rest body)
  (let ((win (gensym))
        (result (gensym)))
    `(let ((,win (get-buffer-window)))
       (switch-to-buffer (slime-repl))
       (end-of-buffer)
       (let ((,result
              (progn ,@body)))
         (other-window
          (find-window-index ,win))
         ,result))))

(defun slime-command (&rest args)
  (in-slime-buffer
   (end-of-buffer)
   (apply #'insert args)
   (slime-repl-return)))

(defun find-window-index (win)
  (let ((count 0)
        (result nil))
    (walk-windows (lambda (w)
                    (when (equal w win)
                      (setf result count))
                    (incf count)))
    result))
