;;;; makeres-graphviz provides graph visualization for makeres target
;;;; graphs.
;;;;
;;;; Copyright 2014-2015 Gary Hollis
;;;;
;;;; This file is part of makeres-graphviz.
;;;;
;;;; makeres-graphviz is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; makeres-graphviz is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-graphviz.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(asdf:defsystem #:cl-ana.makeres-graphviz
  :serial t
  :author "Gary Hollis"
  :description "makeres-graphviz provides graph visualization for
  makeres target graphs."
  :license "GPLv3"
  :depends-on (#:cl-ana.makeres
               #:external-program)
  :components ((:file "package")
               (:file "dot")))
