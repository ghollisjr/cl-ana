(in-package logres)

(defmethod save-target (lid (str string) path)
  (with-open-file (file path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~s~%" str)))
