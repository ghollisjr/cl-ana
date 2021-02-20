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

(defcunion _u-t
    (address  haddr-t)
  (val-size size-t))

(defcstruct h5l-info-t "H5L_info_t"
  (type         h5l-type-t)
  (corder-valid hbool-t)
  (corder       :int64)
  (cset         h5t-cset-t)
  (u            (:union _u-t)))

(defcfun "H5Lcopy" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Copy"
  (src-loc-id  hid-t)
  (src-name    :string)
  (dest-loc-id hid-t)
  (dest-name   :string)
  (lcpl-id     hid-t)
  (lapl-id     hid-t))

(defcfun "H5Lcreate_external" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateExternal"
  (target-file-name :string)
  (target-obj-name  :string)
  (link-loc-id      hid-t)
  (link-name        :string)
  (lcpl-id          hid-t)
  (lapl-id          hid-t))

(defcfun "H5Lcreate_hard" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateHard"
  (obj-loc-id  hid-t)
  (obj-name    :string)
  (link-loc-id hid-t)
  (link-name   :string)
  (lcpl-id     hid-t)
  (lapl-id     hid-t))

(defcfun "H5Lcreate_soft" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateSoft"
  (target-path :string)
  (link-loc-id hid-t)
  (link-name   :string)
  (lcpl-id     hid-t)
  (lapl-id     hid-t))

(defcfun "H5Lcreate_ud" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateUD"
  (link-loc-id hid-t)
  (link-name   :string)
  (link-type   h5l-type-t)
  (udata       :pointer)
  (udata-size  size-t)
  (lcpl-id     hid-t)
  (lapl-id     hid-t))

(defcfun "H5Ldelete" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Delete"
  (loc-id  hid-t)
  (name    :string)
  (lapl-id hid-t))

(defcfun "H5Ldelete_by_idx" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-DeleteByIdx"
  (loc-id      hid-t)
  (group-name  :string)
  (index-filed h5-index-t)
  (order       h5-iter-order-t)
  (n           hsize-t)
  (lapl-id     hid-t))

(defcfun "H5Lexists" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Exists"
  (loc-id  hid-t)
  (name    :string)
  (lapl-id hid-t))

(defcfun "H5Lget_info" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-GetInfo"
  (link-loc-id hid-t)
  (link-name   :string)
  (link-buff   (:pointer (:struct h5l-info-t)))
  (lapl-id     hid-t))

(defcfun "H5Lget_info_by_idx" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-GetInfo"
  (loc-id      hid-t)
  (group-name  :string)
  (index-filed h5-index-t)
  (order       h5-iter-order-t)
  (n           hsize-t)
  (link-val    (:pointer (:struct h5l-info-t)))
  (lapl-id     hid-t))

(defcfun "H5Lget_name_by_idx" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-GetNameByIdx"
  (link-loc-id hid-t)
  (group-name  :string)
  (index-field h5-index-t)
  (order       h5-iter-order-t)
  (n           hsize-t)
  (name        (:pointer :char))
  (size        size-t)
  (lapl-id     hid-t))

(defcfun "H5Lget_val" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-GetVal"
  (link-loc-id  hid-t)
  (link-name    :string)
  (linkval-buff :pointer)
  (size         size-t)
  (lapl-id      hid-t))

(defcfun "H5Lget_val_by_idx" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-GetValByIdx"
  (loc-id      hid-t)
  (group-name  :string)
  (index-field h5-index-t)
  (order       h5-iter-order-t)
  (n           hsize-t)
  (link-val    :pointer)
  (size        size-t)
  (lapl-id     hid-t))

(defcfun "H5Lis_registered" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-IsRegistered"
  (link-cls-id h5l-type-t))

(defcfun "H5Literate" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Iterate"
  (group-id   hid-t)
  (index-type h5-index-t)
  (order      h5-iter-order-t)
  (idx        (:pointer hsize-t))
  (op         :pointer)
  (op-data    :pointer))

(defcfun "H5Literate_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-IterateByName"
  (loc-id     hid-t)
  (group-name (:pointer :char))
  (index-type h5-index-t)
  (order      h5-iter-order-t)
  (idx        (:pointer hsize-t))
  (op         :pointer)
  (op-data    :pointer)
  (lapl-id    hid-t))

(defcfun "H5Lmove" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Move"
  (src-loc-id  hid-t)
  (src-name    :string)
  (dest-loc-id hid-t)
  (dest-name   :string)
  (lcpl-id     hid-t)
  (lapl-id     hid-t))

(defcfun "H5Lregister" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Register"
  (link-class (:pointer (:struct h5l-class-t))))

(defcfun "H5Lunpack_elink_val" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-UnpackELinkVal"
  (ext-link-val (:pointer :char))
  (link-size    size-t)
  (flags        (:pointer :unsigned-int))
  (filename     (:pointer (:pointer :char)))
  (obj-path     (:pointer (:pointer :char))))

(defcfun "H5Lunregister" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Unregister"
  (link-cls-id h5l-type-t))

(defcfun "H5Lvisit" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Visit"
  (group-id   hid-t)
  (index-type h5-index-t)
  (order      h5-iter-order-t)
  (op         :pointer)
  (op-data    :pointer))

(defcfun "H5Lvisit_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-VisitByName"
  (loc-id     hid-t)
  (group-name :string)
  (index-type h5-index-t)
  (order      h5-iter-order-t)
  (op         :pointer)
  (op-data    :pointer)
  (lapl-id    hid-t))
