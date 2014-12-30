;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
;;;; 
;;;; This file is part of cl-ana.
;;;; 
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.package-utils)

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

;;;; Package Groups
;;; A package group is simply a collection of packages.  The concept
;;; is useful for maintaining a high degree of modularity: Instead of
;;; creating a single common package for a set of functionality, each
;;; individual piece of the software can have its own package and then
;;; the combined exported symbols can easily be imported into another
;;; package via a package group.

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
