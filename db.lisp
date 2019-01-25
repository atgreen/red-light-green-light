;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019  Anthony Green <green@moxielogic.com>
;;;                         
;;; rlgl-server is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; rlgl-server is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rlgl-server; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

;; Matcher routines

(defpackage #:rlgl.db
  (:use #:cl)
  (:shadow #:package)
  (:export #:initialize #:record-log #:report-log))

(in-package #:rlgl.db)

(defvar *sqlite-db-filename* nil)

(defun initialize (db &key (sqlite-db-filename nil) (fresh nil))
  (setf *sqlite-db-filename* sqlite-db-filename)
  (let ((db (dbi:connect-cached :sqlite3 :database-name *sqlite-db-filename*)))
    (when fresh
      (dbi:do-sql db "drop table log;"))
    ;;    (let ((query (dbi:prepare db "create table if not exists log (id char(12), version char(40), result varchar(6), report varchar(24) not null, Timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)")))
    (let ((query (dbi:prepare db "create table if not exists log (id char(12), version char(40), report varchar(24) not null, Timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)")))
      (dbi:execute query))))

(defun record-log (player version result report)
  (dbi:do-sql (dbi:connect-cached :sqlite3 :database-name *sqlite-db-filename*)
    (format nil "insert into log(id, version, result, report) values (\"~A\", \"~A\", \"~A\", \"~A\");"
	    player version result report)))

(defun report-log (player)
  (let* ((query (dbi:prepare (dbi:connect-cached :sqlite3 :database-name *sqlite-db-filename*)
			     (format nil "select timestamp, result, version, report from log where id = \"~A\";" player)))
	 (result (dbi:execute query))
	 (fstr (make-array '(0) :element-type 'base-char
                           :fill-pointer 0 :adjustable t)))
      (with-output-to-string (s fstr)
	(loop for row = (dbi:fetch result)
	   while row
	   do (destructuring-bind (j1 time j2 result j3 version j4 report)
		  row
		(local-time:format-timestring
		 s (local-time:universal-to-timestamp
		    (cl-date-time-parser:parse-date-time time))
		 :format local-time:+rfc-1123-format+)
		(format s ": ~A [~A] ~A/doc?id=~A~%" result version rlgl-server:*server-uri* report)))
	fstr)))
