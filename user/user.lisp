;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019, 2020, 2021  Anthony Green <green@moxielogic.com>
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

(in-package #:rlgl.user)

(defclass user ()
  ((id :reader user-id)
   (uuid :reader user-uuid)
   (api-key :reader user-api-key)
   (name :reader user-name)))

(defun make-user (id uuid api-key login)
  (let ((u (make-instance 'user)))
    (setf (slot-value u 'id) id)
    (setf (slot-value u 'uuid) uuid)
    (setf (slot-value u 'api-key) api-key)
    (setf (slot-value u 'name) login)
    u))

(defun find-user-by-keycloak-id-token (db id-json)
  (let ((user (rlgl.db:find-user-by-keycloak-id db
						(cdr (assoc :SUB id-json))
						(cdr (assoc :NAME id-json)))))
    user))
