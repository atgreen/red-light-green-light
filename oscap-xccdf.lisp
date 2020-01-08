;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
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

(in-package :rlgl-server)

;;; MVP OpenSCAP XCCDF Results report parser

(defclass parser/oscap-xccdf (report-parser)
  ()
  (:default-initargs
   :title  "OpenSCAP XCCDF Scan Report"))

(defmethod parse-report ((parser parser/oscap-xccdf) doc)
  (let ((pdoc (plump:parse (flexi-streams:make-flexi-stream
			    (flexi-streams:make-in-memory-input-stream doc)
			    :external-format :utf-8)))
	(tests-fail (list))
	(tests-pass (list)))
    (let ((rows (lquery:$ pdoc "#rule-overview > table > tbody > tr")))
      (loop for row across rows do
	(let ((text (lquery:$ row "td" (text))))
	  (when (eq (length text) 3)
	    (let ((id (aref text 0))
		  (severity (aref text 1))
		  (result (aref text 2)))
	      (cond
		((string= result "fail")
		 (setf tests-fail
		       (cons
			(json:decode-json-from-string
			 (format nil "{ \"report\": \"oscap-xccdf\", \"result\": \"FAIL\", \"id\": \"~A\", \"severity\": \"~A\" }"
				 id severity))
			tests-fail)))
		((string= result "pass")
		 (setf tests-pass
		       (cons
			(json:decode-json-from-string
			 (format nil "{ \"report\": \"oscap-xccdf\", \"result\": \"PASS\", \"id\": \"~A\", \"severity\": \"~A\" }"
				 id severity))
			tests-pass)))))))))
    (append tests-fail tests-pass)))
