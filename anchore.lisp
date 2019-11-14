;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2019  Anthony Green <green@moxielogic.com>
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

;;; MVP Anchore Results report parser

;; ----------------------------------------------------------------------------

(defclass parser/anchore (report-parser)
  ()
  (:default-initargs
   :title  "Anchore Scan Report"))

(defmethod parse-report ((parser parser/anchore) doc)
  (let* ((report (json:decode-json-from-string doc))
	 (tests-pass (list))
	 (tests-fail
	   (let ((vulnerabilities (cdr (assoc :VULNERABILITIES report))))
	     (mapcar (lambda (v)
		       (json:decode-json-from-string
			(format nil "{ \"report\": \"anchore\", \"result\": \"FAIL\", \"id\": \"~A\", \"package name\": \"~A\", \"severity\": \"~A\", \"url\": \"~A\" }"
				(cdr (assoc :VULN v))
				(cdr (assoc :PACKAGE--NAME v))
				(cdr (assoc :SEVERITY v))
				(cdr (assoc :URL v)))))
		     vulnerabilities))))
    (append tests-fail tests-pass)))
