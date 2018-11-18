;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: WEBAPP; Base: 10 -*-

;;; Copyright (C) 2018  Anthony Green <green@moxielogic.com>

;;; Webapp is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; Webapp is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with webapp; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

;;;; package.lisp

(defpackage #:webapp
  (:use #:hunchentoot #:cl)
  (:shadow #:package)
  (:export #:start-webapp #:stop-webapp))

(in-package #:webapp)
