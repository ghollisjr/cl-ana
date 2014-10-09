(in-package logres)

(defmethod save-object (id (fn function) path)
  (format t "Warning: cannot save functions~%")
  nil)

(defmethod load-object ((fn (eql 'function)) path)
  (format t "Warning: cannot load functions~%")
  nil)
