;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019, 2020  Anthony Green <green@moxielogic.com>
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

(defpackage #:rlgl.user
  (:use #:cl)
  (:shadow #:package)
  (:export #:make-user #:find-github-user-by-info))

(in-package #:rlgl.user)

(defclass user ()
  ((id :reader id)))

(defun make-user (id uuid)
  (cons id uuid))

(defun find-github-user-by-info (db github-user-info-string)
  (let* ((user-json (json:decode-json-from-string github-user-info-string))
	 (user (rlgl.db:find-github-user-by-id db (cdr (assoc :ID user-json)))))
    (log:info user)
    user))
