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

;;; See H5Dpublih.h .

(in-package #:hdf5)

(defcfun "H5Dclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Close"
  (dataset-id hid-t))

(defcfun "H5Dcreate1" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Create1"
  (loc-id   hid-t)
  (name     :string)
  (type-id  hid-t)
  (space-id hid-t)
  (dcpl-id  hid-t))

(defcfun "H5Dcreate2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Create2"
  (loc-id   hid-t)
  (name     :string)
  (dtype-id hid-t)
  (space-id hid-t)
  (lcpl-id  hid-t)
  (dcpl-id  hid-t)
  (dapl     hid-t))

(defcfun "H5Dcreate_anon" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-CreateAnon"
  (loc-id   hid-t)
  (type-id  hid-t)
  (space-id hid-t)
  (dcpl-id  hid-t)
  (dapl     hid-t))

(defcfun "H5Dfill" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Fill"
  (fill         :pointer)
  (fill-type-id hid-t)
  (buf          :pointer)
  (buf-type-id  hid-t)
  (space-id     hid-t))

(defcfun "H5Dgather" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Gather"
  (src-space-id hid-t)
  (src-buf      :pointer)
  (type-id      hid-t)
  (dst-buf-size size-t)
  (dst-buf      :pointer)
  (op           :pointer)
  (op-data      :pointer))

(defcfun "H5Dget_access_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-GetAccessPlist"
  (dataset-id hid-t))

(defcfun "H5Dget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-GetCreatePlist"
  (dataset-id hid-t))

(defcfun "H5Dget_offset" haddr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-GetOffset"
  (dataset-id hid-t))

(defcfun "H5Dget_space" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-GetSpace"
  (dataset-id hid-t))

(defcfun "H5Dget_space_status" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-GetSpaceStatus"
  (dataset-id hid-t)
  (status     (:pointer h5d-space-status-t)))

(defcfun "H5Dget_storage_size" hsize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-GetStorageSize"
  (dataset-id hid-t))

(defcfun "H5Dget_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-GetType"
  (dataset-id hid-t))

(defcfun "H5Diterate" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Iterate"
  (buf           :pointer)
  (type-id       hid-t)
  (space-id      hid-t)
  (operator      :pointer)
  (operator-data :pointer))

(defcfun "H5Dopen2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Open2"
  (loc-id  hid-t)
  (name    :string)
  (dapl-id hid-t))

(defcfun "H5Dread" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Read"
  (dataset-id        hid-t)
  (mem-type-id       hid-t)
  (mem-dataspace-id  hid-t)
  (file-dataspace-id hid-t)
  (xfer-plist-id     hid-t)
  (buffer            :pointer))

(defcfun "H5Dscatter" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Scatter"
  (op           :pointer)
  (op-data      :pointer)
  (type-id      hid-t)
  (dst-space-id hid-t)
  (dst-buf      :pointer))

(defcfun "H5Dset_extent" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-SetExtent"
  (dset-id hid-t)
  (size    (:pointer hsize-t)))

(defcfun "H5Dvlen_get_buf_size" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-VLGetBuf"
  (dataset-id hid-t)
  (type-id    hid-t)
  (space-id   hid-t)
  (size       (:pointer hsize-t)))

(defcfun "H5Dvlen_reclaim" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-VLReclaim"
  (type-id  hid-t)
  (space_id hid-t)
  (plist-id hid-t)
  (buf      :pointer))

(defcfun "H5Dwrite" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html#Dataset-Write"
  (datset-id     hid-t)
  (mem-type-id   hid-t)
  (mem-space-id  hid-t)
  (file-space-id hid-t)
  (xfer-plist-id hid-t)
  (buf           :pointer))
