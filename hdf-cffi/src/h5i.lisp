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

(defcfun "H5Iclear_type" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-ClearType"
  (type h5i-type-t)
  (force hbool-t))

(defcfun "H5Idec_ref" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-DecRef"
  (obj-id hid-t))

(defcfun "H5Idec_type_ref" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-DecTypeRef"
  (type h5i-type-t))

(defcfun "H5Idestroy_type" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-DestroyType"
  (type h5i-type-t))

(defcfun "H5Iget_file_id" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetFileId"
  (obj-id hid-t))

(defcfun "H5Iget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetType"
  (obj-id hid-t)
  (name   (:pointer :char))
  (size   size-t))

(defcfun "H5Iget_ref" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetRef"
  (obj-id hid-t))

(defcfun "H5Iget_type" h5i-type-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetType"
  (obj-id hid-t))

(defcfun "H5Iget_type_ref" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetTypeRef"
  (type h5i-type-t))

(defcfun "H5Iinc_ref" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-IncRef"
  (obj-id hid-t))

(defcfun "H5Iinc_type_ref" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-IncTypeRef"
  (type h5i-type-t))

(defcfun "H5Iis_valid" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-IsValid"
  (obj-id hid-t))

(defcfun "H5Inmembers" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-NMembers"
  (type        h5i-type-t)
  (num-members (:pointer hsize-t)))

(defcfun "H5Iobject_verify" :pointer
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-ObjectVerify"
  (id hid-t)
  (id-type h5i-type-t))

(defcfun "H5Iregister" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-Register"
  (type h5i-type-t)
  (object :pointer))

(defcfun "H5Iregister_type" h5i-type-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-RegisterType"
  (hash-size size-t)
  (reserved :unsigned-int)
  (free-func :pointer))

(defcfun "H5Iremove_verify" :pointer
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-RemoveVerify"
  (id hid-t)
  (id-type h5i-type-t))

(defcfun "H5Isearch" :pointer
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-Search"
  (type h5i-type-t)
  (func :pointer)
  (key :pointer))

(defcfun "H5Itype_exists" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-TypeExists"
  (type h5i-type-t))
