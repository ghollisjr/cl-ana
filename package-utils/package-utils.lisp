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

(defvar *package-groups*
  (make-hash-table :test 'equal))

(defun add-package-to-group (package group)
  "Adds the package to the package group given by group."
  (let ((pac (find-package package)))
    (symbol-macrolet ((hash-value (gethash group *package-groups*)))
      (setf hash-value
            (adjoin pac
                    hash-value
                    :test #'equal)))))

(defmacro defpackage-in-group (package-name group &body package-body)
  "Defines a package while placing this package into the group
specified by group.  group can technically be any object, but I like
to stick to keyword symbols.  Any package statements can be used in
the package-body portion."
  (with-gensyms (pac-name)
    `(let ((,pac-name ,package-name))
       (defpackage ,pac-name
         ,@package-body)
       (add-package-to-group ,pac-name ,group))))

(defun use-package-group (group &optional dest-package)
  "Calls shadowing-use-package on each package in group and either the
current package or dest-package."
  (let ((packages (gethash group *package-groups*))
        (dest-pac (if dest-package
                      dest-package
                      *package*)))
    (loop
       for p in packages
       do (shadowing-use-package p dest-pac))))
