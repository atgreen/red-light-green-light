;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL.DB; Base: 10 -*-
;;;
;;; Copyright (C) 2019  Anthony Green <green@moxielogic.com>
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

;; Matcher routines

(in-package #:rlgl.db)

(defclass db/postgresql (db-backend)
  ((postgresql-db-filename
    :initarg :filename
    :reader filename)
   (fresh
    :initarg :fresh
    :initform nil
    :reader fresh))
  (:default-initargs
      :filename (error "Must supply a filename.")))

(defmethod connect-cached ((db db/postgresql))
    (dbi:connect-cached :postgres :database-name (filename db) :username "rlgl" :password "rlgl"))
