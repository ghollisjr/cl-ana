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

(defcfun "H5Sclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-Close"
  (space-id hid-t))

(defcfun "H5Scopy" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-Copy"
  (space-id hid-t))

(defcfun "H5Screate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-Create"
  (types h5s-class-t))

(defcfun "H5Screate_simple" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-CreateSimple"
  (rank         :int)
  (current-dims (:pointer hsize-t))
  (maximum-dims (:pointer hsize-t)))

(defcfun "H5Sdecode" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-Decode"
  (buf (:pointer :unsigned-char)))

(defcfun "H5Sencode" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-Encode"
  (obj-id hid-t)
  (buf    (:pointer :unsigned-char))
  (nalloc (:pointer size-t)))

(defcfun "H5Sextent_copy" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentCopy"
  (dest-space-id   hid-t)
  (source-space-id hid-t))

(defcfun "H5Sextent_equal" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentEqual"
  (space1-id hid-t)
  (space2-id hid-t))

(defcfun "H5Sget_select_bounds" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectBounds"
  (space-id hid-t)
  (start    (:pointer hsize-t))
  (end      (:pointer hsize-t)))

(defcfun "H5Sget_select_elem_npoints" hssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectElemNPoints"
  (space-id hid-t))

(defcfun "H5Sget_select_elem_pointlist" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectElemPointList"
  (space-id   hid-t)
  (startpoint hsize-t)
  (numpoints  hsize-t)
  (buf        (:pointer hsize-t)))

(defcfun "H5Sget_select_hyper_blocklist" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectHyperBlockList"
  (space-id   hid-t)
  (startblock hsize-t)
  (numblocks  hsize-t)
  (buf        (:pointer hsize-t)))

(defcfun "H5Sget_select_hyper_nblocks" hssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectHyperNBlocks"
  (space-id hid-t))

(defcfun "H5Sget_select_npoints" hssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectNpoints"
  (space-id hid-t))

(defcfun "H5Sget_select_type" h5s-sel-type
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-GetSelectType"
  (space-id hid-t))

(defcfun "H5Sget_simple_extent_dims" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentDims"
  (space-id hid-t)
  (dims     (:pointer hsize-t))
  (maxdims  (:pointer hsize-t)))

(defcfun "H5Sget_simple_extent_ndims" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentNdims"
  (space-id hid-t))

(defcfun "H5Sget_simple_extent_npoints" hssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentNpoints"
  (space-id hid-t))

(defcfun "H5Sget_simple_extent_type" H5S-class-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentType"
  (space-id hid-t))

(defcfun "H5Sis_simple" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-IsSimple"
  (space-id hid-t))

(defcfun "H5Soffset_simple" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-OffsetSimple"
  (space-id hid-t)
  (offset   (:pointer hssize-t)))

(defcfun "H5Sselect_all" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectAll"
  (dspace-id         hid-t))

(defcfun "H5Sselect_elements" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectElements"
  (space-id         hid-t)
  (select-operation h5s-seloper-t)
  (num-elements     size-t)
  (coord            (:pointer hsize-t)))

(defcfun "H5Sselect_hyperslab" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectHyperslab"
  (space-id         hid-t)
  (select-operation h5s-seloper-t)
  (start            (:pointer hsize-t))
  (stride           (:pointer hsize-t))
  (count            (:pointer hsize-t))
  (block            (:pointer hsize-t)))

(defcfun "H5Sselect_none" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectNone"
  (space-id hid-t))

(defcfun "H5Sselect_valid" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectValid"
  (space-id hid-t))

(defcfun "H5Sset_extent_none" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SetExtentNone"
  (space-id hid-t))

(defcfun "H5Sset_extent_simple" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SetExtentSimple"
  (space-id     hid-t)
  (rank         :int)
  (current-size (:pointer hsize-t))
  (maximum-size (:pointer hsize-t)))
