;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL.DB; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019, 2020, 2021, 2022  Anthony Green <green@moxielogic.com>
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

(defclass db/sqlite (db-backend)
  ((sqlite-db-filename
    :initarg :filename
    :reader filename)
   (fresh
    :initarg :fresh
    :initform nil
    :reader fresh))
  (:default-initargs
   :sql-insert-log-statement "insert into log(version, colour, report, signature, client_signature, unixtimestamp) values ('~A', '~A', '~A', '~A', '~A', '~A', strftime('%s','now'));"
   :filename (error "Must supply a filename.")))

(defmethod initialize-instance :after ((db db/sqlite) &key)
  (let ((dbc (connect-cached db)))
      (mapc (lambda (command)
	      (dbi:do-sql dbc command))
	    '("create table if not exists users (puk integer primary key autoincrement, user_uuid char(36) not null, created_at integer, unique(user_uuid));"))))

(defmethod connect-cached ((db db/sqlite))
  (dbi:connect-cached :sqlite3 :database-name (filename db)))
