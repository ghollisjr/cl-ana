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

(in-package :cl-ana.quantity)

(defvar +c+
  (* 299792458 :meter (expt :second -1))
  "Speed of light in vacuum")

(defvar +G+
  #q(6.6738480d-11 (:meter 3)
                   (:kilogram -1)
                   (:second -2))
  "Newton's gravitational constant")

(defvar +h+
  (* 6.6260695729d-34 :joule :second)
  "Planck's constant")

(defvar +hbar+
  (* 1.05457172647d-34 :joule :second)
  "Reduced Planck's constant")

(defvar +mu0+
  (* 4 pi
     1d-7
     :newton
     (/ (expt :ampere
              2)))
  "Permeability of free space")

(defvar +eps0+
  (/ (* +mu0+
        (expt +c+ 2)))
  "Permittivity of free space")

(defvar +Z0+
  (* +mu0+ +c+)
  "Impedance of vacuum")

(defvar +k-coulomb+
  (/ (* 4 pi +eps0+))
  "Coulomb's constant")

(defvar +e+
  (* 1.60217656535d-19 :coulomb)
  "Elementary charge")

(defvar +mu-bohr+
  (* 9.2740096820d-24 :joule (/ :tesla))
  "Bohr magneton")

(defvar +a0+
  #q(5.291772109217d-11 :meter)
  "Bohr radius")

(defvar +r-electron+
  (* 2.817940326727d-15 :meter)
  "Classical electron radius")

(defvar +me+
  (* 9.1093829140e-31 :kilogram)
  "Electron mass")

(defvar +alpha+
  (/ (expt +e+ 2)
     (* 4 pi
        +eps0+ +hbar+ +c+))
  "Fine Structure Constant")

(defvar +mp+
  (* 1.67262177774d-27 :kilogram)
  "Proton mass")

(defvar +mn+
  (* 1.67492735174d-27 :kilogram)
  "Neutron Mass")

(defvar +md+
  (* 3.34358348d-27 :kilogram)
  "Deuteron Mass")

(defvar +rydberg+
  (* (expt +alpha+ 2)
     +me+ +c+
     (/ (* 2 +h+)))
  "Rydberg Constant")

(defvar +NA+
  6.0221412927e23
  "Avogadro's Number")

(defvar +k-boltzmann+
  (* 1.380648813d-23 :joule (/ :kelvin))
  "Boltzmann constant")

(defvar +R+
  (* 8.314462175d0 :joule
     (/ (* :kelvin :mole)))
  "Ideal Gas Constant")

(defvar +stefan-boltzmann+
  (* (expt pi 2)
     (/ (expt +k-boltzmann+ 4)
        (* 60
           (expt +hbar+ 3)
           (expt +c+ 2))))
  "Stefan-Boltzmann Constant")

(defvar +graviational-acceleration+
  (* 9.80665d0 :meter (expt :second -2))
  "Standard Gravitational Acceleration")

(defvar +vsound+
  (* 340.29d0 (/ :meter :second)))

(defvar +T0+
  #q(273.15d0 :kelvin)
  "Freezing point of water")
