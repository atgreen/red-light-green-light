;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL.DB; Base: 10 -*-
;;;
;;; Copyright (C) 2019, 2020  Anthony Green <green@moxielogic.com>
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

;; Postgres driver

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
   (password
    :initarg :password
    :reader password)
   (fresh
    :initarg :fresh
    :initform nil
    :reader fresh))
  (:default-initargs
   :db-name "rlgl"
   :host "localhost"
   :port 5432
   :sql-insert-log-statement "insert into log(id, version, colour, report, unixtimestamp) values ('~A', '~A', '~A', '~A', round(extract(epoch from now())));"))

(defmethod initialize-instance :after ((db db/postgresql) &key)
  (let ((dbc (connect-cached db)))
      (mapc (lambda (command)
	      (dbi:do-sql dbc command))
	    '("create extension if not exists \"uuid-ossp\";"
	      "create table if not exists users (puk serial primary key, user_uuid char(36) not null, created_at integer, unique(user_uuid));"))))

(defmethod connect-cached ((db db/postgresql))
  (log:info "establishing postgresql connection at ~A:~A"
	    (host db)
	    (port db))
  (log:info "postgresql host IP: ~A"
	    (sb-bsd-sockets:get-host-by-name (host db)))
  (dbi:connect-cached :postgres :database-name (db-name db)
				:host (host db)
				:port (port db)
				:username "rlgl" :password (password db)))
