(in-package logres)

;; Default behavior for tables: do nothing, they're usually stored via
;; files which should go in the work/ directory.

(defmethod save-object ((tab table) path)
  nil)

(defmethod load-object ((tab table) path)
  nil)
