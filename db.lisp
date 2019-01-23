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
  (:export #:initialize #:log-evaluation #:report-log))

(in-package #:rlgl.db)

(defvar *pool* nil)

(defun initialize (db &key (sqlite-db-filename nil) (fresh nil))
  (setf *pool*
	(pooler:make-pool
	 :item-maker (ecase db
		       (:sqlite3
			#'(lambda () (dbi:connect :sqlite3 :database-name sqlite-db-filename))))
	 :item-destroyer #'(lambda (item) (dbi:disconnect item))))
  
  (pooler:with-pool (db *pool*)
    (when fresh
      (dbi:do-sql db "drop table log;"))
    (dbi:do-sql db "create table if not exists log (id char(12), report varchar(24) not null, Timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)")))

(defun log-evaluation (player report)
  (pooler:with-pool (db *pool*)
    (dbi:do-sql db
      (format nil "insert into log(id, report) values (\"~A\", \"~A\");" player report))))

(defun report-log (player)
  (pooler:with-pool (db *pool*)
    (let* ((query (dbi:prepare db (format nil "select timestamp, report from log where id = \"~A\";" player)))
	   (result (dbi:execute query))
	   (fstr (make-array '(0) :element-type 'base-char
                            :fill-pointer 0 :adjustable t)))
      (with-output-to-string (s fstr)
	(loop for row = (dbi:fetch result)
	   while row
	   do (format s "~A~%" row)))
      fstr)))



