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

;;; See H5Gpublic.h .

(in-package #:hdf5)

(defcfun "H5Gclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-Close"
  (group-id hid-t))

(defcfun "H5Gcreate1" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-Create1"
  (loc-id    hid-t)
  (name      :string)
  (size-hint size-t))

(defcfun "H5Gcreate2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-Create2"
  (loc-id  hid-t)
  (name    :string)
  (lcpl-id hid-t)
  (gcpl-id hid-t)
  (gapl-id hid-t))

(defcfun "H5Gcreate_anon" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-CreateAnon"
  (loc-id  hid-t)
  (gcpl-id hid-t)
  (gapl-id hid-t))

(defcfun "H5Gget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-GetCreatePlist"
  (group-id hid-t))

(defcfun "H5Gget_info" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-GetInfo"
  (group-id   hid-t)
  (group-info (:pointer (:struct h5g-info-t))))

(defcfun "H5Gget_info_by_idx" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-GetInfoByIdx"
  (loc-id     hid-t)
  (group-name :string)
  (index-type h5-index-t)
  (order      h5-iter-order-t)
  (n          hsize-t)
  (group-info (:pointer (:struct h5g-info-t)))
  (lapl-id    hid-t))

(defcfun "H5Gget_info_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-GetInfoByName"
  (loc-id     hid-t)
  (group-name :string)
  (group-info (:pointer (:struct h5g-info-t)))
  (lapl-id    hid-t))

(defcfun "H5Gopen1" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-Open1"
  (loc-id hid-t)
  (name   :string))

(defcfun "H5Gopen2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html#Group-Open2"
  (loc-id hid-t)
  (name   :string)
  (gapl   hid-t))
