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

;; Database routines

(in-package #:rlgl.db)

(defclass db-backend ()
  ((key :initarg :key :reader key)
   (make-user-fn :initarg :make-user-fn :reader make-user-fn)
   (make-api-key-fn :initarg :make-api-key-fn :reader make-api-key-fn)
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
              "drop table if exists labels;"
	      "drop table if exists api_keys;")))

    (mapc (lambda (command)
	    (dbi:do-sql dbc command))
	  '("create table if not exists log (version char(40), colour varchar(6), report varchar(24) not null, signature char(140) not null, client_signature char(140) not null, unixtimestamp integer);"
	    "create table if not exists policy_bound_api_keys (api_key char(31) not null, policy varchar(256) not null);"
            "create table if not exists labels (puk integer, report char(13), key varchar(64), value varchar(256));"
	    "create table if not exists api_keys (puk integer, api_key char(31) not null);"))))

(defmethod record-log ((db db-backend) api-key version result report signature client-signature labels)
  (let ((stmt (format nil (sql-insert-log-statement db)
		      version result report signature client-signature)))
    (log:info stmt)
    (let ((connection (connect-cached db)))
      (dbi:do-sql connection stmt)
      (dolist (kv labels)
        (dbi:do-sql connection (format nil "insert into labels(puk, report, key, value) values ('~A', '~A', '~A', '~A');"
                                       report
                                       (find-puk-by-api-key db api-key)
                                       (str:substring 0 64 (string (car kv))) (str:substring 0 256 (cdr kv))))))))

(defmethod report-log ((db db-backend) server-uri labels)
  (let* ((connection (connect-cached db))
         (report-sets (mapcar (lambda (kv)
                                (let* ((query (dbi:prepare connection
                                                           (format nil "select report from labels where key = '~A' and value = '~A'"
                                                                   (str:substring 0 64 (string (car kv))) (str:substring 0 256 (cdr kv)))))
                                       (result (dbi:execute query))
                                       (reports (loop for row = (dbi:fetch result)
                                                      while row
                                                      collect (destructuring-bind (j1 report)
                                                                  row
                                                                report))))
                                  (fset:convert 'fset:set reports)))
                              labels))
         (reports (if (eq (length report-sets) 1)
                      (car report-sets)
                      (fset:reduce #'fset:intersection report-sets)))
         (fstr (make-array '(0) :element-type 'base-char
                                :fill-pointer 0 :adjustable t)))

    (with-output-to-string (s fstr)
      (fset:do-set (report reports)
        (let* ((query (dbi:prepare (connect-cached db)
			           (format nil "select unixtimestamp, colour, version, client_signature from log where report = '~A';" report)))
	       (result (dbi:execute query)))
	  (loop for row = (dbi:fetch result)
	        while row
	        do (destructuring-bind (j1 time j2 result j3 version j4 client-signature)
		       row
		     (local-time:format-timestring
		      s (local-time:unix-to-timestamp time)
		      :format local-time:+rfc-1123-format+)
		     (format s ": ~A [~5A] ~A/doc?id=~A ~A~%" result version server-uri report client-signature))))))
    fstr))

(defmethod find-signature-by-report ((db db-backend) report)
  (let* ((query (dbi:prepare (connect-cached db)
			     (format nil "select signature from log where report = '~A';" report)))
	 (result (dbi:fetch (dbi:execute query))))
    (str:trim (getf result :|signature|))))

(defmethod find-client-signature-by-report ((db db-backend) report)
  (let* ((query (dbi:prepare (connect-cached db)
			     (format nil "select client_signature from log where report = '~A';" report)))
	 (result (dbi:fetch (dbi:execute query))))
    (str:trim (getf result :|client_signature|))))

(defmethod find-puk-by-api-key ((db db-backend) api-key)
  (let* ((query (dbi:prepare (connect-cached db)
			     (format nil "select puk from api_keys where api_key = '~A';" api-key)))
	 (result (dbi:fetch (dbi:execute query))))
    (getf result :|puk|)))

(defmethod find-policy-bound-api-key ((db db-backend) api-key)
  (let* ((query (dbi:prepare (connect-cached db)
			     (format nil "select policy from policy_bound_api_keys where api_key = '~A';" api-key)))
	 (result (dbi:fetch (dbi:execute query))))
    (getf result :|policy|)))

(defmethod register-test-api-key ((db db-backend) api-key)
  (dbi:do-sql (connect-cached db)
    (format nil "insert into api_keys(puk, api_key) values (-1, '~A');" api-key)))

(defmethod find-user-by-keycloak-id ((db db-backend) user-uuid preferred-username)
  (let* ((query (dbi:prepare (connect-cached db)
			     (format nil "select puk from users where user_uuid = '~A';" user-uuid)))
	 (result (dbi:fetch (dbi:execute query)))
	 (user (let ((puk (getf result :|puk|)))
		 (log:info "found user puk ~A" puk)
		 (and puk
		      (let* ((query (dbi:prepare (connect-cached db)
						 (format nil "select api_key from api_keys where puk = ~A;" puk)))
			     (junk (log:info (dbi:fetch (dbi:execute query))))
			     (api-key (getf (dbi:fetch (dbi:execute query)) :|api_key|)))
			(log:info "~A" result)
			(funcall (make-user-fn db) puk user-uuid api-key preferred-username))))))
    (if (null user)
	(progn
	  (log:info "registering new user ~A" user-uuid)
	  (dbi:do-sql (connect-cached db)
	    (format nil "insert into users(user_uuid, created_at) values ('~A', ~A);"
		    user-uuid (local-time:timestamp-to-unix (local-time:now))))
	  (let* ((query (dbi:prepare (connect-cached db)
				     (format nil "select puk from users where user_uuid = '~A';" user-uuid)))
		 (newpuk (getf (dbi:fetch (dbi:execute query)) :|puk|)))
	    (log:info "created user puk ~A" newpuk)
	    (dbi:do-sql (connect-cached db)
	      (format nil "insert into api_keys(puk, api_key) values (~A, '~A');" newpuk (funcall (make-api-key-fn db)))))
          (find-user-by-keycloak-id db user-uuid preferred-username))
	user)))

(defmethod register-policy-bound-api-key ((db db-backend) api-key policy)
  (dbi:do-sql (connect-cached db)
    (format nil "insert into policy_bound_api_keys(api_key, policy) values ('~A', '~A');" api-key policy))
  (dbi:do-sql (connect-cached db)
    (format nil "insert into users(api_key, created_at) values ('~A', ~A);" api-key (local-time:timestamp-to-unix (local-time:now))))
  api-key)
