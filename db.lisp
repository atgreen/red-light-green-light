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
	    (let ((query (dbi:prepare dbc command)))
	      (dbi:execute query)))
	  '("create table if not exists log (id char(12), version char(40), colour varchar(6), report varchar(24) not null, unixtimestamp integer)"
	    "create table if not exists users (user_id integer, github_id integer)"
	    "create table if not exists api_keys (user_id integer, api_key char(40))"))))

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

(defmethod find-github-user-by-id ((db db-backend) github-user-id)
  (let* ((query (dbi:prepare (connect-cached db)
			     (format nil "select api_key from users where github_id = '~A';" github-user-id)))
	 (result (dbi:fetch (dbi:execute query)))
	 (fstr (make-array '(0) :element-type 'base-char
				:fill-pointer 0 :adjustable t)))
    (unless result
      (log:info "github user ~A is not registered" github-user-id))
    (log:info result)))

