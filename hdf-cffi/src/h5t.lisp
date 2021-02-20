;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

(in-package #:hdf5)

(defcfun "H5Tarray_create2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-ArrayCreate2"
  (base-type-id hid-t)
  (rank         :uint)
  (dims         (:pointer hsize-t)))

(defcfun "H5Tclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Close"
  (dtype-id hid-t))

(defcfun "H5Tcommit2" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Commit"
  (loc-id   hid-t)
  (name     :string)
  (dtype-id hid-t)
  (lcpl-id  hid-t)
  (tcpl-id  hid-t)
  (tapl-id  hid-t))

(defcfun "H5Tcommit_anon" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-CommitAnon"
  (loc-id   hid-t)
  (dtype-id hid-t)
  (tcpl-id  hid-t)
  (tapl-id  hid-t))

(defcfun "H5Tcommitted" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Committed"
  (dtype-id hid-t))

(defcfun "H5Tcompiler_conv" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-CompilerConv"
  (src-id hid-t)
  (dst-id hid-t))

(defcfun "H5Tconvert" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Convert"
  (src-type   hid-t)
  (dest-type  hid-t)
  (nelmts     size-t)
  (buf        :pointer)
  (background :pointer)
  (plist      hid-t))

(defcfun "H5Tcopy" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Copy"
  (dtype-id hid-t))

(defcfun "H5Tcreate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Create"
  (class h5t-class-t)
  (size  size-t))

(defcfun "H5Tdecode" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Decode"
  (buf (:pointer :unsigned-char)))

(defcfun "H5Tdetect_class" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-DetectClass"
  (dtype-id hid-t)
  (class    h5t-class-t))

(defcfun "H5Tencode" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Encode"
  (obj-id hid-t)
  (buf    (:pointer :unsigned-char))
  (nalloc (:pointer size-t)))

(defcfun "H5Tenum_create" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-EnumCreate"
  (dtype-id hid-t))

(defcfun "H5Tenum_insert" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-EnumInsert"
  (dtype-id hid-t)
  (name     :string)
  (value    :pointer))

(defcfun "H5Tenum_nameof" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-EnumNameOf"
  (dtype-id hid-t)
  (value    :pointer)
  (name     (:pointer :char))
  (size     size-t))

(defcfun "H5Tenum_valueof" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-EnumValueOf"
  (dtype-id hid-t)
  (name     (:pointer :char))
  (value    (:pointer)))

(defcfun "H5Tequal" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Equal"
  (dtype-id1 hid-t)
  (dtype-id2 hid-t))

(defcfun "H5Tfind" :pointer
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Find"
  (src-id hid-t)
  (dst-id hid-t)
  (pcdata (:pointer (:pointer (:struct h5t-cdata-t)))))

(defcfun "H5Tget_array_dims2" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetArrayDims2"
  (adtype-id hid-t)
  (dims      (:pointer hsize-t)))

(defcfun "H5Tget_array_ndims" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetArrayNdims"
  (adtype-id hid-t))

(defcfun "H5Tget_class" h5t-class-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetClass"
  (dtype-id hid-t))

(defcfun "H5Tget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetCreatePlist"
  (dtype-id hid-t))

(defcfun "H5Tget_cset" h5t-cset-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetCset"
  (dtype-id hid-t))

(defcfun "H5Tget_ebias" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetEbias"
  (dtype-id hid-t))

(defcfun "H5Tget_fields" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetFields"
  (dtype-id hid-t)
  (spos     (:pointer size-t))
  (epos     (:pointer size-t))
  (esize    (:pointer size-t))
  (mpos     (:pointer size-t))
  (msize    (:pointer size-t)))

(defcfun "H5Tget_inpad" h5t-pad-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetFields"
  (dtype-id hid-t))

(defcfun "H5Tget_member_class" h5t-class-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberClass"
  (cdtype-id hid-t)
  (member-no :unsigned-int))

(defcfun "H5Tget_member_index" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberIndex"
  (dtype-id   hid-t)
  (field-name :string))

(defcfun "H5Tget_member_name" (:pointer :char)
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberName"
  (dtype-id  hid-t)
  (field-idx :uint))

(defcfun "H5Tget_member_offset" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberOffset"
  (dtype-id hid-t)
  (memb-no :uint))

(defcfun "H5Tget_member_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberType"
  (dtype-id  hid-t)
  (field-idx :uint))

(defcfun "H5Tget_member_value" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberValue"
  (dtype-id hid-t)
  (memb-no  :unsigned-int)
  (value    :pointer))

(defcfun "H5Tget_native_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetNativeType"
  (dtype-id  hid-t)
  (direction h5t-dir-t))

(defcfun "H5Tget_nmembers" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetNmembers"
  (dtype-id hid-t))

(defcfun "H5Tget_norm" h5t-norm-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetNorm"
  (dtype-id hid-t))

(defcfun "H5Tget_offset" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetOffset"
  (dtype-id hid-t))

(defcfun "H5Tget_order" h5t-order-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetOrder"
  (dtype-id hid-t))

(defcfun "H5Tget_pad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetPad"
  (dtype-id hid-t)
  (lsb      (:pointer h5t-pad-t))
  (msb      (:pointer h5t-pad-t)))

(defcfun "H5Tget_precision" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetPrecision"
  (dtype-id hid-t))

(defcfun "H5Tget_sign" h5t-sign-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSign"
  (dtype-id hid-t))

(defcfun "H5Tget_size" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSize"
  (dtype-id hid-t))

(defcfun "H5Tget_strpad" h5t-str-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetStrpad"
  (dtype-id hid-t))

(defcfun "H5Tget_super" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSuper"
  (type hid-t))

(defcfun "H5Tget_tag" (:pointer :char)
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetTag"
  (dtype-id hid-t))

(defcfun "H5Tinsert" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Insert"
  (dtype-id hid-t)
  (name     :string)
  (offset   size-t)
  (field-id hid-t))

(defcfun "H5Tis_variable_str" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-IsVariableString"
  (dtype-id hid-t))

(defcfun "H5Tlock" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Lock"
  (dtype-id  hid-t))

(defcfun "H5Topen2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Open2"
  (loc-id  hid-t)
  (name    :string)
  (tapl-id hid-t))

(defcfun "H5Tpack" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Pack"
  (dtype-id  hid-t))

(defcfun "H5Tregister" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Register"
  (type   h5t-pers-t)
  (name   (:pointer :char))
  (src-id hid-t)
  (dst-id hid-t)
  (func   :pointer))

(defcfun "H5Tset_cset" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetCset"
  (dtype-id hid-t)
  (cset     h5t-cset-t))

(defcfun "H5Tset_ebias" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetEbias"
  (dtype-id hid-t)
  (ebias    size-t))

(defcfun "H5Tset_fields" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetFields"
  (dtype-id hid-t)
  (spos     size-t)
  (epos     size-t)
  (esize    size-t)
  (mpos     size-t)
  (msize    size-t))

(defcfun "H5Tset_inpad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetInpad"
  (dtype-id hid-t)
  (inpad    h5t-pad-t))

(defcfun "H5Tset_norm" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetNorm"
  (dtype-id hid-t)
  (norm     h5t-norm-t))

(defcfun "H5Tset_offset" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetOffset"
  (dtype-id hid-t)
  (offset   size-t))

(defcfun "H5Tset_order" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetOrder"
  (dtype-id hid-t)
  (order    h5t-order-t))

(defcfun "H5Tset_pad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetPad"
  (dtype-id hid-t)
  (lsb      h5t-pad-t)
  (msb      h5t-pad-t))

(defcfun "H5Tset_precision" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetPrecision"
  (dtype-id  hid-t)
  (precision size-t))

(defcfun "H5Tset_sign" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetSign"
  (dtype-id hid-t)
  (sign     h5t-sign-t))

(defcfun "H5Tset_size" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetSize"
  (dtype-id hid-t)
  (size     size-t))

(defcfun "H5Tset_strpad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetStrpad"
  (dtype-id hid-t)
  (cset     h5t-str-t))

(defcfun "H5Tset_tag" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetTag"
  (dtype-id hid-t)
  (tag      :string))

(defcfun "H5Tunregister" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Unregister"
  (type   h5t-pers-t)
  (name   :string)
  (src-id hid-t)
  (dst-id hid-t)
  (func   :pointer))

(defcfun "H5Tvlen_create" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-VLCreate"
  (base-type-id hid-t))

;; these are NOT constants; instead, these are alias to the global variables that are set by H5T__init_package() .
;; For example, H5T_IEEE_F32BE is an alias to H5T_IEEE_F32BE_g .

(defmacro pseudo-constant (name)
  `(defcvar (,(lispify (subseq name 0 (- (length name) 2)) 'constant) ,name) hid-t))

(pseudo-constant       "H5T_IEEE_F32BE_g")
(pseudo-constant       "H5T_IEEE_F32LE_g")
(pseudo-constant       "H5T_IEEE_F64BE_g")
(pseudo-constant       "H5T_IEEE_F64LE_g")

(pseudo-constant         "H5T_STD_I8BE_g")
(pseudo-constant         "H5T_STD_I8LE_g")
(pseudo-constant        "H5T_STD_I16BE_g")
(pseudo-constant        "H5T_STD_I16LE_g")
(pseudo-constant        "H5T_STD_I32BE_g")
(pseudo-constant        "H5T_STD_I32LE_g")
(pseudo-constant        "H5T_STD_I64BE_g")
(pseudo-constant        "H5T_STD_I64LE_g")
(pseudo-constant         "H5T_STD_U8BE_g")
(pseudo-constant         "H5T_STD_U8LE_g")
(pseudo-constant        "H5T_STD_U16BE_g")
(pseudo-constant        "H5T_STD_U16LE_g")
(pseudo-constant        "H5T_STD_U32BE_g")
(pseudo-constant        "H5T_STD_U32LE_g")
(pseudo-constant        "H5T_STD_U64BE_g")
(pseudo-constant        "H5T_STD_U64LE_g")
(pseudo-constant         "H5T_STD_B8BE_g")
(pseudo-constant         "H5T_STD_B8LE_g")
(pseudo-constant        "H5T_STD_B16BE_g")
(pseudo-constant        "H5T_STD_B16LE_g")
(pseudo-constant        "H5T_STD_B32BE_g")
(pseudo-constant        "H5T_STD_B32LE_g")
(pseudo-constant        "H5T_STD_B64BE_g")
(pseudo-constant        "H5T_STD_B64LE_g")
(pseudo-constant      "H5T_STD_REF_OBJ_g")
(pseudo-constant  "H5T_STD_REF_DSETREG_g")

(pseudo-constant       "H5T_UNIX_D32BE_g")
(pseudo-constant       "H5T_UNIX_D32LE_g")
(pseudo-constant       "H5T_UNIX_D64BE_g")
(pseudo-constant       "H5T_UNIX_D64LE_g")

(pseudo-constant             "H5T_C_S1_g")

(pseudo-constant       "H5T_FORTRAN_S1_g")

;; /*
;;  * These types are for Intel CPU's.  They are little endian with IEEE
;;  * floating point.
;;  */
(define-symbol-macro +H5T-INTEL-I8+ +H5T-STD-I8LE+)
(define-symbol-macro +H5T-INTEL-I16+ +H5T-STD-I16LE+)
(define-symbol-macro +H5T-INTEL-I32+ +H5T-STD-I32LE+)
(define-symbol-macro +H5T-INTEL-I64+ +H5T-STD-I64LE+)
(define-symbol-macro +H5T-INTEL-U8+ +H5T-STD-U8LE+)
(define-symbol-macro +H5T-INTEL-U16+ +H5T-STD-U16LE+)
(define-symbol-macro +H5T-INTEL-U32+ +H5T-STD-U32LE+)
(define-symbol-macro +H5T-INTEL-U64+ +H5T-STD-U64LE+)
(define-symbol-macro +H5T-INTEL-B8+ +H5T-STD-B8LE+)
(define-symbol-macro +H5T-INTEL-B16+ +H5T-STD-B16LE+)
(define-symbol-macro +H5T-INTEL-B32+ +H5T-STD-B32LE+)
(define-symbol-macro +H5T-INTEL-B64+ +H5T-STD-B64LE+)
(define-symbol-macro +H5T-INTEL-F32+ +H5T-IEEE-F32LE+)
(define-symbol-macro +H5T-INTEL-F64+ +H5T-IEEE-F64LE+)

;; /*
;;  * These types are for DEC Alpha CPU's.  They are little endian with IEEE
;;  * floating point.
;;  */
(define-symbol-macro +H5T-ALPHA-I8+ +H5T-STD-I8LE+)
(define-symbol-macro +H5T-ALPHA-I16+ +H5T-STD-I16LE+)
(define-symbol-macro +H5T-ALPHA-I32+ +H5T-STD-I32LE+)
(define-symbol-macro +H5T-ALPHA-I64+ +H5T-STD-I64LE+)
(define-symbol-macro +H5T-ALPHA-U8+ +H5T-STD-U8LE+)
(define-symbol-macro +H5T-ALPHA-U16+ +H5T-STD-U16LE+)
(define-symbol-macro +H5T-ALPHA-U32+ +H5T-STD-U32LE+)
(define-symbol-macro +H5T-ALPHA-U64+ +H5T-STD-U64LE+)
(define-symbol-macro +H5T-ALPHA-B8+ +H5T-STD-B8LE+)
(define-symbol-macro +H5T-ALPHA-B16+ +H5T-STD-B16LE+)
(define-symbol-macro +H5T-ALPHA-B32+ +H5T-STD-B32LE+)
(define-symbol-macro +H5T-ALPHA-B64+ +H5T-STD-B64LE+)
(define-symbol-macro +H5T-ALPHA-F32+ +H5T-IEEE-F32LE+)
(define-symbol-macro +H5T-ALPHA-F64+ +H5T-IEEE-F64LE+)

;; /*
;;  * These types are for MIPS cpu's commonly used in SGI systems. They are big
;;  * endian with IEEE floating point.
;;  */
(define-symbol-macro +H5T-MIPS-I8+ +H5T-STD-I8BE+)
(define-symbol-macro +H5T-MIPS-I16+ +H5T-STD-I16BE+)
(define-symbol-macro +H5T-MIPS-I32+ +H5T-STD-I32BE+)
(define-symbol-macro +H5T-MIPS-I64+ +H5T-STD-I64BE+)
(define-symbol-macro +H5T-MIPS-U8+ +H5T-STD-U8BE+)
(define-symbol-macro +H5T-MIPS-U16+ +H5T-STD-U16BE+)
(define-symbol-macro +H5T-MIPS-U32+ +H5T-STD-U32BE+)
(define-symbol-macro +H5T-MIPS-U64+ +H5T-STD-U64BE+)
(define-symbol-macro +H5T-MIPS-B8+ +H5T-STD-B8BE+)
(define-symbol-macro +H5T-MIPS-B16+ +H5T-STD-B16BE+)
(define-symbol-macro +H5T-MIPS-B32+ +H5T-STD-B32BE+)
(define-symbol-macro +H5T-MIPS-B64+ +H5T-STD-B64BE+)
(define-symbol-macro +H5T-MIPS-F32+ +H5T-IEEE-F32BE+)
(define-symbol-macro +H5T-MIPS-F64+ +H5T-IEEE-F64BE+)

(pseudo-constant          "H5T_VAX_F32_g")
(pseudo-constant          "H5T_VAX_F64_g")

(if (not (zerop +char-min+))
    (define-symbol-macro +h5t-native-char+ +H5T-NATIVE-SCHAR+)
    (define-symbol-macro +h5t-native-char+ +H5T-NATIVE-UCHAR+))

(pseudo-constant     "H5T_NATIVE_SCHAR_g")
(pseudo-constant     "H5T_NATIVE_UCHAR_g")
(pseudo-constant     "H5T_NATIVE_SHORT_g")
(pseudo-constant    "H5T_NATIVE_USHORT_g")
(pseudo-constant       "H5T_NATIVE_INT_g")
(pseudo-constant      "H5T_NATIVE_UINT_g")
(pseudo-constant      "H5T_NATIVE_LONG_g")
(pseudo-constant     "H5T_NATIVE_ULONG_g")
(pseudo-constant     "H5T_NATIVE_LLONG_g")
(pseudo-constant    "H5T_NATIVE_ULLONG_g")
(pseudo-constant     "H5T_NATIVE_FLOAT_g")
(pseudo-constant    "H5T_NATIVE_DOUBLE_g")
;; disabled at the moment --- where is the definition of H5_SIZEOF_LONG_DOUBLE ?
;; (if (not (zerop +H5_SIZEOF_LONG_DOUBLE+))
;; (defcvar (+H5T_NATIVE_LDOUBLE+      "H5T_NATIVE_LDOUBLE_g")      hid-t)
(pseudo-constant        "H5T_NATIVE_B8_g")
(pseudo-constant       "H5T_NATIVE_B16_g")
(pseudo-constant       "H5T_NATIVE_B32_g")
(pseudo-constant       "H5T_NATIVE_B64_g")
(pseudo-constant    "H5T_NATIVE_OPAQUE_g")
(pseudo-constant     "H5T_NATIVE_HADDR_g")
(pseudo-constant     "H5T_NATIVE_HSIZE_g")
(pseudo-constant    "H5T_NATIVE_HSSIZE_g")
(pseudo-constant      "H5T_NATIVE_HERR_g")
(pseudo-constant     "H5T_NATIVE_HBOOL_g")

(pseudo-constant "H5T_NATIVE_INT8_g")
(pseudo-constant "H5T_NATIVE_UINT8_g")
(pseudo-constant "H5T_NATIVE_INT_LEAST8_g")
(pseudo-constant "H5T_NATIVE_UINT_LEAST8_g")
(pseudo-constant "H5T_NATIVE_INT_FAST8_g")
(pseudo-constant "H5T_NATIVE_UINT_FAST8_g")

(pseudo-constant     "H5T_NATIVE_INT16_g")
(pseudo-constant    "H5T_NATIVE_UINT16_g")
(pseudo-constant "H5T_NATIVE_INT_LEAST16_g")
(pseudo-constant "H5T_NATIVE_UINT_LEAST16_g")
(pseudo-constant "H5T_NATIVE_INT_FAST16_g")
(pseudo-constant "H5T_NATIVE_UINT_FAST16_g")

(pseudo-constant         "H5T_NATIVE_INT32_g")
(pseudo-constant        "H5T_NATIVE_UINT32_g")
(pseudo-constant   "H5T_NATIVE_INT_LEAST32_g")
(pseudo-constant  "H5T_NATIVE_UINT_LEAST32_g")
(pseudo-constant    "H5T_NATIVE_INT_FAST32_g")
(pseudo-constant   "H5T_NATIVE_UINT_FAST32_g")

(pseudo-constant         "H5T_NATIVE_INT64_g")
(pseudo-constant        "H5T_NATIVE_UINT64_g")
(pseudo-constant   "H5T_NATIVE_INT_LEAST64_g")
(pseudo-constant  "H5T_NATIVE_UINT_LEAST64_g")
(pseudo-constant    "H5T_NATIVE_INT_FAST64_g")
(pseudo-constant   "H5T_NATIVE_UINT_FAST64_g")
