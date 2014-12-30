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

(in-package :cl-ana.histogram)

(defun sparse->contiguous (histogram)
  "converts a sparse histogram into a contiguous one.  Note that this
is dangerous in cases where sparse histograms are actually necessary."
  (if (typep histogram 'sparse-histogram)
      (let* ((dim-spec-plists
              (hist-dim-specs
               histogram))
             (bin-values
              (hist-bin-values histogram))
             (hist
              (make-contiguous-hist dim-spec-plists)))
        (loop
           for datum in bin-values
           do (hist-insert hist (rest datum) (first datum)))
        hist)
      histogram))

(defun contiguous->sparse (histogram)
  "converts a sparse histogram into a contiguous one.  Note that this
is dangerous in cases where sparse histograms are actually necessary."
  (if (typep histogram 'contiguous-histogram)
      (let* ((dim-spec-plists
              (hist-dim-specs
               histogram))
             (bin-values
              (hist-bin-values histogram))
             (hist
              (make-sparse-hist dim-spec-plists)))
        (loop
           for datum in bin-values
           do (hist-insert hist (rest datum) (first datum)))
        hist)
      histogram))
