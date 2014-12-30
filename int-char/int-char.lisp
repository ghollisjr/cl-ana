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

(in-package :cl-ana.int-char)

(defparameter *printable-ascii-codes*
  (list
   (cons 32 #\Space)
   (cons 33 #\!)
   (cons 34 #\")
   (cons 35 #\#)
   (cons 36 #\$)
   (cons 37 #\%)
   (cons 38 #\&)
   (cons 39 #\')
   (cons 40 #\()
   (cons 41 #\))
   (cons 42 #\*)
   (cons 43 #\+)
   (cons 44 #\,)
   (cons 45 #\-)
   (cons 46 #\.)
   (cons 47 #\/)
   (cons 48 #\0)
   (cons 49 #\1)
   (cons 50 #\2)
   (cons 51 #\3)
   (cons 52 #\4)
   (cons 53 #\5)
   (cons 54 #\6)
   (cons 55 #\7)
   (cons 56 #\8)
   (cons 57 #\9)
   (cons 58 #\:)
   (cons 59 #\;)
   (cons 60 #\<)
   (cons 61 #\=)
   (cons 62 #\>)
   (cons 63 #\?)
   (cons 64 #\@)
   (cons 65 #\A)
   (cons 66 #\B)
   (cons 67 #\C)
   (cons 68 #\D)
   (cons 69 #\E)
   (cons 70 #\F)
   (cons 71 #\G)
   (cons 72 #\H)
   (cons 73 #\I)
   (cons 74 #\J)
   (cons 75 #\K)
   (cons 76 #\L)
   (cons 77 #\M)
   (cons 78 #\N)
   (cons 79 #\O)
   (cons 80 #\P)
   (cons 81 #\Q)
   (cons 82 #\R)
   (cons 83 #\S)
   (cons 84 #\T)
   (cons 85 #\U)
   (cons 86 #\V)
   (cons 87 #\W)
   (cons 88 #\X)
   (cons 89 #\Y)
   (cons 90 #\Z)
   (cons 91 #\[)
   (cons 92 #\\)
   (cons 93 #\])
   (cons 94 #\^)
   (cons 95 #\_)
   (cons 96 #\`)
   (cons 97 #\a)
   (cons 98 #\b)
   (cons 99 #\c)
   (cons 100 #\d)
   (cons 101 #\e)
   (cons 102 #\f)
   (cons 103 #\g)
   (cons 104 #\h)
   (cons 105 #\i)
   (cons 106 #\j)
   (cons 107 #\k)
   (cons 108 #\l)
   (cons 109 #\m)
   (cons 110 #\n)
   (cons 111 #\o)
   (cons 112 #\p)
   (cons 113 #\q)
   (cons 114 #\r)
   (cons 115 #\s)
   (cons 116 #\t)
   (cons 117 #\u)
   (cons 118 #\v)
   (cons 119 #\w)
   (cons 120 #\x)
   (cons 121 #\y)
   (cons 122 #\z)
   (cons 123 #\{)
   (cons 124 #\|)
   (cons 125 #\})
   (cons 126 #\~)))

(defparameter *ascii-map*
  (make-hash-table :test 'equal))

(loop
   for (code . char) in *printable-ascii-codes*
   do (setf (gethash code *ascii-map*)
            char))

(defun int-char (integer)
  (gethash integer *ascii-map*))
