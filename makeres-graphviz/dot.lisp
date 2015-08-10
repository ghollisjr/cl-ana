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

(in-package :cl-ana.makeres-graphviz)

(defun dot-compile (path &key
                           (if-exists :error)
                           (target-table (target-table)))
  "Writes target graph into a file located at path.  Returns path of
dot output file."
  (let ((*print-pretty* nil))
    (with-open-file (file path
                          :direction :output
                          :if-exists if-exists)
      (format file "digraph \"~a\" {~%"
              (project))
      (loop
         for id being the hash-keys in target-table
         for tar being the hash-values in target-table
         do (let ((deps (target-deps tar)))
              (loop
                 for d in deps
                 do (format file "  \"~a\" -> \"~a\";~%"
                            d id))))
      (format file "}~%")
      path)))

(defun dot->ps (from-path to-path)
  "Runs dot command to convert dot code in from-path to a ps file at
to-path"
  (run "dot"
       (list from-path
             "-Tps"
             "-o"
             to-path)))

(defun dot->png (from-path to-path)
  "Runs dot command to convert dot code in from-path to a png file at
to-path"
  (run "dot"
       (list from-path
             "-Tpng"
             "-o"
             to-path)))
