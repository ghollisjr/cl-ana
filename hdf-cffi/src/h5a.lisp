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

(defcfun "H5Aclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Close"
  (attr-id hid-t))

(defcfun "H5Acreate1" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Create1"
  (loc-id    hid-t)
  (attr-name :string)
  (type-id   hid-t)
  (space-id  hid-t)
  (acpl-id   hid-t))

(defcfun "H5Acreate2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Create2"
  (loc-id    hid-t)
  (attr-name :string)
  (type-id   hid-t)
  (space-id  hid-t)
  (acpl-id   hid-t)
  (aapl-id   hid-t))

(defcfun "H5Acreate_by_name" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-CreateByName"
  (loc-id    hid-t)
  (obj-name  :string)
  (attr-name :string)
  (type-id   hid-t)
  (space-id  hid-t)
  (acpl-id   hid-t)
  (aapl-id   hid-t)
  (lapl-id   hid-t))

(defcfun "H5Adelete" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Delete"
  (loc-id    hid-t)
  (attr-name :string))

(defcfun "H5Adelete_by_idx" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-DeleteByIdx"
  (loc-id   hid-t)
  (obj-name :string)
  (idx-type h5-index-t)
  (order    h5-iter-order-t)
  (n        hsize-t)
  (lapl-id  hid-t))

(defcfun "H5Adelete_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-DeleteByName"
  (loc-id    hid-t)
  (obj-name  :string)
  (attr-name :string)
  (lapl-id   hid-t))

(defcfun "H5Aexists" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Exists"
  (obj-id    hid-t)
  (attr-name :string))

(defcfun "H5Aexists_by_name" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-ExistsByName"
  (obj-id    hid-t)
  (obj-name  :string)
  (attr-name :string)
  (lapl-id   hid-t))

(defcfun "H5Aget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetCreatePlist"
  (attr-id hid-t))

(defcfun "H5Aget_info" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetInfo"
  (attr-id hid-t)
  (info    (:pointer (:struct h5a-info-t))))

(defcfun "H5Aget_info_by_idx" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetInfoByIdx"
  (loc-id   hid-t)
  (obj-name :string)
  (idx-type h5-index-t)
  (order    h5-iter-order-t)
  (n        hsize-t)
  (info     (:pointer (:struct H5A-info-t)))
  (lapl-id  hid-t))

(defcfun "H5Aget_info_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetInfoByName"
  (loc-id    hid-t)
  (obj-name  :string)
  (attr-name :string)
  (info      (:pointer (:struct h5a-info-t)))
  (lapl-id   hid-t))

(defcfun "H5Aget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetName"
  (attr-id  hid-t)
  (buf-size size-t)
  (buf      (:pointer :char)))

(defcfun "H5Aget_name_by_idx" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetNameByIdx"
  (loc-id   hid-t)
  (obj-name :string)
  (idx-type h5-index-t)
  (order    h5-iter-order-t)
  (n        hsize-t)
  (name     (:pointer :char))
  (size     size-t)
  (lapl-id  hid-t))

(defcfun "H5Aget_space" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetSpace"
  (attr-id hid-t))

(defcfun "H5Aget_storage_size" hsize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetStorageSize"
  (attr-id hid-t))

(defcfun "H5Aget_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetType"
  (attr-id hid-t))

(defcfun "H5Aiterate2" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Iterate2"
  (obj-id   hid-t)
  (idx-type h5-index-t)
  (order    h5-iter-order-t)
  (n        (:pointer hsize-t))
  (op       :pointer)
  (op-data  :pointer))

(defcfun "H5Aiterate_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-IterateByName"
  (loc-id   hid-t)
  (obj-name :string)
  (idx-type h5-index-t)
  (order    h5-iter-order-t)
  (n        (:pointer hsize-t))
  (op       :pointer)
  (op_data  :pointer)
  (lapd-id  hid-t))

(defcfun "H5Aopen" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Open"
  (obj-id    hid-t)
  (attr-name :string)
  (aapl-id   hid-t))

(defcfun "H5Aopen_by_idx" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-OpenByIdx"
  (loc-id   hid-t)
  (obj-name :string)
  (idx-type h5-index-t)
  (order    h5-iter-order-t)
  (n        hsize-t)
  (aapl-id  hid-t)
  (lapd-id  hid-t))

(defcfun "H5Aopen_by_name" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-OpenByName"
  (loc-id    hid-t)
  (obj-name  :string)
  (attr-name :string)
  (aapl-id   hid-t)
  (lapl-id   hid-t))

(defcfun "H5Aread" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Read"
  (attr-id     hid-t)
  (mem-type-id hid-t)
  (buf         :pointer))

(defcfun "H5Arename" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Rename"
  (loc-id        hid-t)
  (old-attr-name :string)
  (new-attr-name :string))

(defcfun "H5Arename_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-RenameByName"
  (loc-id        hid-t)
  (obj-name      :string)
  (old-attr-name :string)
  (new-attr-name :string)
  (lapl-id       hid-t))

(defcfun "H5Awrite" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Write"
  (attr-id     hid-t)
  (mem-type-id hid-t)
  (buf         :pointer))
