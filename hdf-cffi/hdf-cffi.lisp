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

(in-package :cl-ana.hdf-cffi)

(define-foreign-library hdf5
  (:unix (:or (:default "libhdf5")
              "/usr/lib/i386-linux-gnu/hdf5/serial/libhdf5.so"
              "/usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.so"))
  (t (:default "libhdf5")))

(use-foreign-library hdf5)

;;; hdf types:

(defctype size-t :uint)
(defctype hid-t :int)
(defctype herr-t :int)
(defctype hsize-t :uint64)
(defctype hssize-t :int64) ; just a signed version of hsize_t
(defctype htri-t :int) ; hdf's boolean, used for h5tequal among others

;; enums:

(defcenum h5t-class-t
  (:H5T-NO-CLASS    -1)
  (:H5T-INTEGER      0)
  (:H5T-FLOAT        1)
  (:H5T-TIME         2)
  (:H5T-STRING       3)
  (:H5T-BITFIELD     4)
  (:H5T-OPAQUE       5)
  (:H5T-COMPOUND     6)
  (:H5T-REFERENCE    7)
  (:H5T-ENUM         8)
  (:H5T-VLEN         9)
  (:H5T-ARRAY        10))

(defcenum h5t-direction-t
  (:H5T-DIR-DEFAULT     0)
  (:H5T-DIR-ASCEND      1)
  (:H5T-DIR-DESCEND     2))

;; There are a bunch more of these which don't have a set value, so
;; they'd need to be grabbed by compiled C code (I guess?).  Luckily
;; almost every time we select some part of the set we want to just
;; select it with :H5S-SELECT-SET
(defcenum h5s-seloper-t
  (:H5S-SELECT-NOOP   -1)
  (:H5S-SELECT-SET     0))

;;; hdf constants:

;; H5S_UNLIMITED: 2^64-1
(defconstant +H5S-UNLIMITED+ 18446744073709551615)

;; H5S_ALL: 0
(defconstant +H5S-ALL+ 0)

;; H5F_ACC_TRUNC
(defconstant +H5F-ACC-TRUNC+ 2) ;; we'll see if it works

;; H5F_ACC_RDONLY
(defconstant +H5F-ACC-RDONLY+ 0)

;; H5F_ACC_RDWR
(defconstant +H5F-ACC-RDWR+ 1)

;; H5P_DEFAULT
(defconstant +H5P-DEFAULT+ 0)

;; H5P_DATASET_CREATE
(defconstant +H5P-DATASET-CREATE+ 150994953)

;; H5T type constants:

(defconstant +H5T-NATIVE-CHAR+ 50331656)
(defconstant +H5T-NATIVE-UCHAR+ 50331657)
(defconstant +H5T-NATIVE-SHORT+ 50331658)
(defconstant +H5T-NATIVE-USHORT+ 50331659)
(defconstant +H5T-NATIVE-INT+ 50331660)
(defconstant +H5T-NATIVE-UINT+ 50331661)
(defconstant +H5T-NATIVE-LONG+ 50331662)
(defconstant +H5T-NATIVE-ULONG+ 50331663)
(defconstant +H5T-NATIVE-LLONG+ 50331688)
(defconstant +H5T-NATIVE-ULLONG+ 50331689)
(defconstant +H5T-NATIVE-FLOAT+ 50331690)
(defconstant +H5T-NATIVE-DOUBLE+ 50331691)
(defconstant +H5T-COMPOUND+ 6)

;; Identifiers (macros from H5public.h)
(defconstant +H5F-OBJ-FILE+ 1)
(defconstant +H5F-OBJ-DATASET+ 2)
(defconstant +H5F-OBJ-GROUP+ 4)
(defconstant +H5F-OBJ-DATATYPE+ 8)
(defconstant +H5F-OBJ-ATTR+ 16)
(defconstant +H5F-OBJ-ALL+ 31)

;;; hdf-cffi interface utilities:

(defparameter *hdf-cffi-type-map*
  (list
   (cons +H5T-NATIVE-CHAR+ :char)
   (cons +H5T-NATIVE-UCHAR+ :uchar)
   (cons +H5T-NATIVE-SHORT+ :short)
   (cons +H5T-NATIVE-USHORT+ :ushort)
   (cons +H5T-NATIVE-INT+ :int)
   (cons +H5T-NATIVE-UINT+ :uint)
   (cons +H5T-NATIVE-LLONG+ :long-long)
   (cons +H5T-NATIVE-ULLONG+ :ullong)
   (cons +H5T-NATIVE-FLOAT+ :float)
   (cons +H5T-NATIVE-DOUBLE+ :double)))

(defun cffi-native-type (hdf-native-type)
  (cdr (find hdf-native-type *hdf-cffi-type-map*
	     :key #'car
	     :test (lambda (t1 t2)
                     (let ((cmp (h5tequal t1 t2)))
                       (if (zerop cmp)
                           nil
                           t))))))

(defun hdf-native-type (cffi-native-type)
  (car (rassoc cffi-native-type *hdf-cffi-type-map*)))

;;; hdf functions:

(defcfun "H5Aclose" herr-t
  (attr-id hid-t))

(defcfun "H5open" herr-t)

(defcfun "H5close" herr-t)

(defcfun "H5Fcreate" hid-t
  (filename :string)
  (flags :uint)
  (fcpl-id hid-t)
  (fapl-id hid-t))

(defcfun "H5Fopen" hid-t
  (name :string)
  (flags :uint)
  (fapl-id hid-t))

(defcfun "H5Fget_obj_count" hssize-t
  (file-id hid-t)
  (types :uint))

(defcfun "H5Fget_obj_ids" hssize-t
  (file-id hid-t)
  (types :uint)
  (max-objs size-t)
  (obj-id-list :pointer)) ; *hid-t

(defcfun "H5Pcreate" hid-t
  (cls-id hid-t))

(defcfun "H5Pset_chunk" herr-t
  (plist hid-t)
  (ndims :int)
  (dim :pointer)) ;; const hsize_t*

(defcfun "H5Pset_deflate" herr-t
  (plist-id hid-t)
  (level :uint))

(defcfun "H5Pget_chunk" :int
  (plist hid-t)
  (max-ndims :int)
  (dims :pointer)) ; hsize_t*

(defcfun "H5Tarray_create2" hid-t
  (base-type-id hid-t)
  (rank :uint)
  (dims :pointer)) ;; const hsize_t dims[rank]

(defcfun "H5Tcreate" hid-t
  (class h5t-class-t)
  (size size-t))

(defcfun "H5Tclose" herr-t
  (dtype-id hid-t))

(defcfun "H5Tinsert" herr-t
  (dtype-id hid-t)
  (name :string)
  (offset size-t)
  (field-id hid-t))

(defcfun "H5Dcreate1" hid-t
  (loc-id hid-t)
  (name :string)
  (type-id hid-t)
  (space-id hid-t)
  (dcpl-id hid-t))

(defcfun "H5Dopen2" hid-t
  (loc-id hid-t)
  (name :string)
  (dapl-id hid-t))

(defcfun "H5Dclose" herr-t
  (dataset-id hid-t))

(defcfun "H5Dwrite" herr-t
  (datset-id hid-t)
  (mem-type-id hid-t)
  (mem-space-id hid-t)
  (file-space-id hid-t)
  (xfer-plist-id hid-t)
  (buf :pointer))

(defcfun "H5Dread" herr-t
  (dataset-id hid-t)
  (mem-type-id hid-t)
  (mem-dataspace-id hid-t)
  (file-dataspace-id hid-t)
  (xfer-plist-id hid-t)
  (buffer :pointer)) ; void*

(defcfun "H5Dget_space" hid-t
  (dataset-id hid-t))

(defcfun "H5Dget_type" hid-t
  (dataset-id hid-t))

(defcfun "H5Dget_create_plist" hid-t
  (dataset-id hid-t))

(defcfun "H5Dset_extent" herr-t
  (dset-id hid-t)
  (size :pointer)) ; const hsize_t size[]

(defcfun "H5Fclose" herr-t
  (file-id hid-t))

(defcfun "H5Screate_simple" hid-t
  (rank :int)
  (current-dims :pointer) ; const hsize_t*
  (maximum-dims :pointer)) ; const hsize_t*

(defcfun "H5Sclose" herr-t
  (space-id hid-t))

(defcfun "H5Sselect_hyperslab" herr-t
  (space-id hid-t)
  (select-operation h5s-seloper-t)
  (start :pointer) ; const hsize_t*
  (stride :pointer) ; const hsize_t*
  (count :pointer) ; const hsize_t*
  (blck :pointer)) ; const hsize_t*

(defcfun "H5Sget_simple_extent_ndims" :int
  (space-id hid-t))

(defcfun "H5Sget_simple_extent_dims" :int
  (space-id hid-t)
  (dims :pointer) ; hsize_t*
  (maxdims :pointer)) ; hsize_t*

(defcfun "H5Tget_class" h5t-class-t
  (dtype-id hid-t))

(defcfun "H5Tget_super" hid-t
  (type hid-t))

(defcfun "H5Tget_native_type" hid-t
  (dtype-id hid-t)
  (direction h5t-direction-t))

(defcfun "H5Tget_size" size-t
  (dtype-id hid-t))

(defcfun "H5Tget_array_ndims" :int
  (adtype-id hid-t))

(defcfun "H5Tget_array_dims2" :int
  (adtype-id hid-t)
  (dims :pointer)) ; hsize_t array

(defcfun "H5Tget_nmembers" :int
  (dtype-id hid-t))

(defcfun "H5Tget_member_type" hid-t
  (dtype-id hid-t)
  (field-idx :uint))

(defcfun "H5Tget_member_name" :string
  (dtype-id hid-t)
  (field-idx :uint))

(defcfun "H5Tget_member_offset" size-t
  (dtype-id hid-t)
  (memb-no :uint))

(defcfun "H5Tequal" htri-t
  (dtype-id1 hid-t)
  (dtype-id2 hid-t))

;;; Group functions:

(defcfun "H5Gcreate2" hid-t
  (file hid-t)
  (name :string)
  (lcpl-id hid-t)
  (gcpl-id hid-t)
  (gapl-id hid-t))

(defcfun "H5Gcreate1" hid-t
  (file hid-t)
  (name :string)
  (size size-t))

(defcfun "H5Gclose" herr-t
  (group-id hid-t))
