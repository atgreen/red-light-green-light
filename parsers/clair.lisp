;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-PARSERS; Base: 10 -*-
;;;
;;; Copyright (C) 2019, 2020, 2021  Anthony Green <green@moxielogic.com>
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

(in-package :rlgl-parsers)

;;; MVP Clair Results report parser

;; ----------------------------------------------------------------------------

(defclass parser/clair (report-parser)
  ()
  (:default-initargs
   :title  "Clair Scan Report"
   :doctype "text"))


(defmethod parse-report ((parser parser/clair) doc)
  (let* ((report (json:decode-json-from-source (flexi-streams:make-flexi-stream
						(flexi-streams:make-in-memory-input-stream doc)
						:external-format :utf-8)))
	 (tests-pass (list))
	 (tests-fail
	   (let ((vulnerabilities (cdr (assoc :VULNERABILITIES report))))
	     (mapcar (lambda (v)
		       (json:decode-json-from-string
			(format nil "{ \"report\": \"clair\", \"result\": \"FAIL\", \"id\": \"~A\", \"url\": \"~A\" }"
				(cdr (assoc :VULNERABILITY v))
				(cdr (assoc :LINK v)))))
		     vulnerabilities))))
    (append tests-fail tests-pass)))
