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

;;; MVP DejaGnu Results report parser

;; ----------------------------------------------------------------------------

(defclass parser/dejagnu (report-parser)
  ()
  (:default-initargs
   :title  "DejaGnu Summary Report"
   :doctype "text"))

(defmethod parse-report ((parser parser/dejagnu) doc)
  (let ((tests (list))
	(host "UNKNOWN")
	(target "UNKNOWN")
	(in (flexi-streams:make-flexi-stream
	     (flexi-streams:make-in-memory-input-stream doc)
	     :external-format :utf-8)))
    (loop for line = (read-line in nil)
	  while line do
	    (setf tests
		  (cond
		    ((str:starts-with? "Native configuration is " line)
		     (setf host (str:substring 24 nil line))
		     (setf target (str:substring 24 nil line))
		     tests)
		    ((str:starts-with? "Host   is" line)
		     (setf host (str:substring 10 nil line))
		     tests)
		    ((str:starts-with? "Target is" line)
		     (setf target (str:substring 10 nil line))
		     tests)
		    ((str:starts-with? "FAIL:" line)
		     (cons
		      (json:decode-json-from-string
		       (format nil "{ \"report\": \"dejagnu\", \"result\": \"FAIL\", \"host\": \"~A\", \"target\": \"~A\", \"id\": \"~A\" }"
			       host target (rlgl-util:escape-json-string
					    (str:substring 6 nil line))))
		      tests))
		    ((str:starts-with? "XFAIL:" line)
		     (cons
		      (json:decode-json-from-string
		       (format nil "{ \"report\": \"dejagnu\", \"result\": \"XFAIL\", \"host\": \"~A\", \"target\": \"~A\", \"id\": \"~A\" }"
			       host target (rlgl-util:escape-json-string
					    (str:substring 7 nil line))))
		      tests))
		    ((str:starts-with? "XPASS:" line)
		     (cons
		      (json:decode-json-from-string
		       (format nil "{ \"report\": \"dejagnu\", \"result\": \"XPASS\", \"host\": \"~A\", \"target\": \"~A\", \"id\": \"~A\" }"
			       host target (rlgl-util:escape-json-string
					    (str:substring 7 nil line))))
		      tests))
		    (t tests))))
    tests))
