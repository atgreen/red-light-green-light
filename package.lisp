;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-

;;; Copyright (C) 2018, 2019  Anthony Green <green@moxielogic.com>

;;; Rlgl-Server is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; Rlgl-Server is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rlgl-server; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

;;;; package.lisp

(defpackage #:rlgl-server
  (:use #:snooze #:cl #:matcher #:policy #:spinneret)
  (:shadow #:package)
  (:export #:start-rlgl-server #:stop-rlgl-server
	   #:parser/oscap-oval))

(in-package #:rlgl-server)
