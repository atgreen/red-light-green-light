;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL.DB; Base: 10 -*-
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

;; Matcher routines

(in-package #:rlgl.db)

(defclass db/sqlite (db-backend)
  ((sqlite-db-filename
    :initarg :filename
    :reader filename)
   (sql-table-init
    :initarg :sql-table-init
    :reader sql-table-init)
   (fresh
    :initarg :fresh
    :initform nil
    :reader fresh))
  (:default-initargs
   :sql-table-init "create table if not exists log (id char(12), version char(40), colour varchar(6), report varchar(24) not null, unixtimestamp integer)"
   :filename (error "Must supply a filename.")))

(defmethod connect-cached ((db db/sqlite))
  (dbi:connect-cached :sqlite3 :database-name (filename db)))
