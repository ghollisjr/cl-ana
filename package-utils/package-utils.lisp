(in-package :package-utils)

(defun shadowing-use-package (from-package &optional to-package)
  "shadowing-imports all the exported symbols from gmath into the
  current package"
  (let ((from-pac (find-package from-package))
	(to-pac (if to-package
                    (find-package to-package)
                    *package*)))
    (do-external-symbols (s from-pac)
      (let ((sym (find-symbol (string s) from-pac)))
	(unintern sym to-pac)
	(shadowing-import sym
			  to-pac)))))
