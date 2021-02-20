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

(defcfun "H5PLget_loading_state" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5PL.html#Plugin-GetLoadingState"
  (plugin-flags (:pointer :int)))

(defcfun "H5PLset_loading_state" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5PL.html#Plugin-SetLoadingState"
  (plugin-type :int))
