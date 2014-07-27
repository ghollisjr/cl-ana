(in-package logres)

(defmethod save-object ((h histogram) path)
  (with-open-hdf-file (file path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (write-histogram h file "/histogram")))

(defmethod load-object ((type (eql 'histogram)) path)
  (with-open-hdf-file (file path
                            :direction :input
                            :if-does-not-exist :error)
    (read-histogram file "/histogram")))
