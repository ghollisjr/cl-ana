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

(in-package :cl-ana.makeres-table)

;;;; Defines gensym tables which allows the reuse of gensyms
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gsym-tables*
    (make-hash-table :test 'equal)
    "Map from id to gsym table")

  ;; structure for storing gsym table info
  (defstruct gsym-table
    symbols
    available-symbols)

  (defun gsym (id)
    (when (not (gethash id *gsym-tables*))
      (setf (gethash id *gsym-tables*)
            (make-gsym-table :symbols (list (gensym))
                             :available-symbols nil)))
    (symbol-macrolet
        ((gsym-tab
          (gethash id *gsym-tables*))
         (available (gsym-table-available-symbols gsym-tab))
         (all (gsym-table-symbols gsym-tab)))
      (if available
          (pop available)
          (let ((gsym (gensym)))
            (push gsym all)
            gsym))))

  (defun clrgsym (id)
    "Frees gsyms for reuse"
    (symbol-macrolet ((gsym-tab (gethash id *gsym-tables*)))
      (when gsym-tab
        (setf (gsym-table-available-symbols gsym-tab)
              (copy-list (gsym-table-symbols gsym-tab)))))
    nil))
