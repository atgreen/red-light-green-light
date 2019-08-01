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
  ((db-name
    :initarg :db-name
    :reader db-name)
   (host
    :initarg :host
    :reader host)
   (port
    :initarg :port
    :reader port)
   (fresh
    :initarg :fresh
    :initform nil
    :reader fresh))
  (:default-initargs
   :db-name "rlgl"
   :host "localhost"
   :port 5432))

(defmethod connect-cached ((db db/postgresql))
  (log:info "postgresql connect-cached")
  (log:info "db ~A" db)
  (log:info "db db-name ~A" (slot-value db 'db-name))
  (log:info "db host ~A" (slot-value db 'host))
  (log:info "db port ~A" (slot-value db 'port))
  (dbi:connect-cached :postgres :database-name (db-name db)
				:host (host db)
				:port (port db)
				:username "rlgl" :password "c0p0$g0g0"))
