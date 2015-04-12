;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2015 Gary Hollis
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

(in-package :cl-ana.makeres-block)

(defvar *project->bid->body*
  (make-hash-table :test 'equal)
  "Map from project to block id (symbol block appended to list of ids
defined by block) to the body defining the block")

(defmacro defresblock ((&rest ids) &body body)
  "Macro defining a body which, when executed, will result in multiple
targets having their values set."
  `(progn
     (when (not (gethash (project) *project->bid->body*))
       (setf (gethash (project) cl-ana.makeres-block::*project->bid->body*)
             (make-hash-table :test 'equal)))
     (setf (gethash ',ids
                    (gethash (project)
                             cl-ana.makeres-block::*project->bid->body*))
           ',body)
     ,@(loop
          for id in ids
          collecting `(defres ,id (makeres-block ,@body)))))

(defun blocktrans (target-table)
  "Target table transformation supporting the definition of blocks
which will result in multiple targets being computed at once."
  (flet ((copy-table (table)
           (let ((result (make-hash-table :test 'equal)))
             (loop
                for id being the hash-keys in table
                for target being the hash-values in table
                do (setf (gethash id result)
                         (copy-target target)))
             result)))
    (let ((result-table
           (copy-table target-table))
          (bid->body (gethash (project) *project->bid->body*)))
      (loop
         for bid being the hash-keys in bid->body
         for body being the hash-values in bid->body
         do (let ((tmpid (gensym)))
              (setf (gethash tmpid result-table)
                    (make-target tmpid
                                 `(progn ,@body)))
              (loop
                 for id in bid
                 do (setf (target-expr (gethash id result-table))
                          `(progn
                             (res ,tmpid)
                             (resfn ',id))))))
      result-table)))
