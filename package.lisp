;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019  Anthony Green <green@moxielogic.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;;; package.lisp

(defpackage #:rlgl-server
  (:use #:snooze #:cl #:matcher #:policy #:spinneret)
  (:shadow #:package)
  (:export #:start-rlgl-server #:stop-rlgl-server
	   #:parser/anchore
	   #:parser/aqua
	   #:parser/clair
	   #:parser/dejagnu
	   #:parser/junit
	   #:parser/oscap-oval
	   #:parser/oscap-xccdf
	   #:db
	   #:db/sqlite
	   #:db/postgresql
	   #:storage/local
	   #:storage/s3
	   #:*server-uri*))

(in-package #:rlgl-server)
