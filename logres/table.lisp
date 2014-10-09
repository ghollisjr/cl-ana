(in-package logres)

;; Default behavior for tables: do nothing, they're usually stored via
;; files which should go in the work/ directory.

(defmethod save-target (id (tab table) path)
  nil)

(defmethod load-target ((type (eql 'table)) path)
  nil)

;; reusable-table technically not a table:

(defmethod save-target (id (tab reusable-table) path)
  nil)

(defmethod load-target ((type (eql 'reusable-table)) path)
  nil)
