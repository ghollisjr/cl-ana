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
;;;; help @hdfgroup.org.

(in-package #:hdf5)

(defcfun "H5Rcreate" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-Create"
  (ref      :pointer)
  (loc-id   hid-t)
  (name     :string)
  (ref-type h5r-type-t)
  (space-id hid-t))

(progn
  (defmacro h5rdereference-gen ()
    (let ((rm-url "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-Dereference")
          (fn-name-v8 "H5Rdereference")
          (fn-name-v10 "H5Rdereference2"))
      (if (foreign-symbol-pointer fn-name-v10)
          `(defcfun (,fn-name-v10 h5rdereference) hid-t
             ,rm-url
             (dataset  hid-t)
             (oapl-id  hid-t)
             (ref-type h5r-type-t)
             (ref      :pointer))
          `(defcfun (,fn-name-v8 h5rdereference) hid-t
             ,rm-url
             (dataset  hid-t)
             (ref-type h5r-type-t)
             (ref      :pointer)))))
  (h5rdereference-gen))

(defcfun "H5Rget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-GetName"
  (loc-id   hid-t)
  (ref-type h5r-type-t)
  (ref      :pointer)
  (name     (:pointer :char))
  (size     size-t))

(defcfun "H5Rget_obj_type2" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-GetObjType2"
  (loc-id   hid-t)
  (ref-type h5r-type-t)
  (ref      :pointer)
  (obj-type (:pointer h5o-type-t)))

(defcfun "H5Rget_region" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-GetRegion"
  (dataset  hid-t)
  (ref-type h5r-type-t)
  (ref      :pointer))
