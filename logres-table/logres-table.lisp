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

(in-package :cl-ana.logres-table)

(defun load-project-tables ()
  "Loads physical table targets from stored results in work/ directory
for project.  Should always be run after load-project."
  (loop
     for id being the hash-keys in (target-table)
     for tar being the hash-values in (target-table)
     do (let ((expr (target-expr tar)))
          (when (cl-ana.makeres-table::tab? expr)
            (destructuring-bind (progn
                                  (tab source inits opener &rest body))
                expr
              (format t "Loading table ~a~%"
                      id)
              (let ((opener (eval opener))) ; necessary eval
                (handler-case (setresfn id (funcall opener :read))
                  (error () (unsetresfn id)))))))))

(defun table-target? (id)
  (let ((tar (gethash id (target-table))))
    (and (target-stat tar)
         (or (cl-ana.makeres-table::tab? (target-expr tar))
             (cl-ana.makeres-table::ltab? (target-expr tar))
             (typep (target-val tar) 'table)
             (typep (target-val tar) 'reusable-table)))))
