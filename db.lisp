;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL.DB; Base: 10 -*-
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

;; Matcher routines

(defpackage #:rlgl.db
  (:use #:cl)
  (:shadow #:package)
  (:export #:record-log #:report-log #:db/sqlite #:db/postgresql
	   #:find-github-user-by-id))

(in-package #:rlgl.db)

(defclass db-backend ()
  ((key :initarg :key :reader key)
   (sql-insert-log-statement
    :initarg :sql-insert-log-statement
    :reader sql-insert-log-statement)))

(defmethod initialize-instance :after ((db db-backend) &key)
  (let ((dbc (connect-cached db)))
    (when (fresh db)
      (mapc (lambda (command)
	      (dbi:do-sql dbc command))
	    '("drop table if exists log;"
	      "drop table if exists users;"
	      "drop table if exists api_keys;")))
    (mapc (lambda (command)
	    (dbi:do-sql dbc command))
	  '("create table if not exists log (id char(12), version char(40), colour varchar(6), report varchar(24) not null, unixtimestamp integer);"
	    "create table if not exists api_keys (puk integer, api_key char(36) not null);"))))

(defmethod record-log ((db db-backend) player version result report)
  (let ((stmt (format nil (sql-insert-log-statement db)
		      player version result report)))
    (log:info stmt)
    (dbi:do-sql (connect-cached db) stmt)))

(defmethod report-log ((db db-backend) player)
  (let* ((query (dbi:prepare (connect-cached db)
			     (format nil "select unixtimestamp, colour, version, report from log where id = '~A';" player)))
	 (result (dbi:execute query))
	 (fstr (make-array '(0) :element-type 'base-char
                           :fill-pointer 0 :adjustable t)))
      (with-output-to-string (s fstr)
	(loop for row = (dbi:fetch result)
	      while row
	      do (destructuring-bind (j1 time j2 result j3 version j4 report)
		     row
		   (local-time:format-timestring
		    s (local-time:unix-to-timestamp time)
		    :format local-time:+rfc-1123-format+)
		   (format s ": ~A [~5A] ~A/doc?id=~A~%" result version rlgl-server:*server-uri* report)))
	fstr)))

(defun make-user (id uuid api-key)
  (log:info "make-user ~A/~A/~A" id uuid api-key)
  (list id uuid api-key))

(defmethod find-github-user-by-id ((db db-backend) github-user-id)
  (let* ((query (dbi:prepare (connect-cached db)
			     (format nil "select puk, user_uuid from users where github_id = '~A';" github-user-id)))
	 (result (dbi:fetch (dbi:execute query)))
	 (user (let ((puk (getf result :|puk|)))
		 (log:info "found user puk ~A" puk)
		 (and puk
		      (let* ((query (dbi:prepare (connect-cached db)
						 (format nil "select api_key from api_keys where puk = ~A;" puk)))
			     (junk (log:info (dbi:fetch (dbi:execute query))))
			     (api-key (getf (dbi:fetch (dbi:execute query)) :|api_key|)))
			(log:info "~A" result)
			(make-user puk (getf result :|user_uuid|) api-key))))))
    (if (null user)
	(let ((user-uuid (uuid:make-v4-uuid)))
	  (log:info "registering new user ~A" user-uuid)
	  (dbi:do-sql (connect-cached db)
	    (format nil "insert into users(user_uuid, github_id) values ('~A', '~A');" user-uuid github-user-id))
	  (let* ((query (dbi:prepare (connect-cached db)
				     (format nil "select puk from users where github_id = ~A;" github-user-id)))
		 (newpuk (getf (dbi:fetch (dbi:execute query)) :|puk|)))
	    (log:info "created user puk ~A" newpuk)
	    (dbi:do-sql (connect-cached db)
	      (format nil "insert into api_keys(puk, api_key) values (~A, '~A');" newpuk (rlgl.api-key:make-api-key))))
	  (find-github-user-by-id db github-user-id))
	user)))
