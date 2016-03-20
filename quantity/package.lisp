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

(defpackage #:cl-ana.quantity
  (:use :cl
        :alexandria
        :cl-ana.list-utils
        :cl-ana.macro-utils
        :cl-ana.symbol-utils
        :cl-ana.error-propogation)
  (:export
   ;; Unit:
   :define-unit
   ;; Quantity:
   :quantity
   :quantity-scale
   :quantity-unit
   ;; Quantity methods:
   :define-quantity-methods
   ;; Temperature
   :convert-temperature
   ;; General unit conversion
   :convert-units
   ;; Metric prefix functions:
   :yotta
   :zetta
   :exa
   :peta
   :tera
   :giga
   :mega
   :kilo
   :hecto
   :deca
   :deci
   :centi
   :milli
   :micro
   :nano
   :pico
   :femto
   :atto
   :zepto
   :yocto
   ;; Binary prefix functions:
   :binary-kilo
   :binary-mega
   :binary-giga
   :binary-tera
   ;; Constants:
   :+c+
   :+G+
   :+h+
   :+hbar+
   :+mu0+
   :+eps0+
   :+Z0+
   :+k-coulomb+
   :+e+
   :+mu-bohr+
   :+a0+
   :+r-electron+
   :+me+
   :+alpha+
   :+mp+
   :+mn+
   :+md+
   :+rydberg+
   :+NA+
   :+k-boltzmann+
   :+R+
   :+stefan-boltzmann+
   :+graviational-acceleration+
   :+vsound+
   :+T0+
   ;; Misc.
   :reader-macro-units->quantity))

(cl-ana.gmath:use-gmath :cl-ana.quantity)
