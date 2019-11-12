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

;;; MVP DejaGnu Results report parser

;; ----------------------------------------------------------------------------

(defclass parser/dejagnu (report-parser)
  ()
  (:default-initargs
   :title  "DejaGnu Summary Report"))

(defmethod parse-report ((parser parser/dejagnu) doc)
  (let ((tests (list))
	(host "UNKNOWN"))
    (with-input-from-string (in doc)
      (loop for line = (read-line in nil)
	    while line do
	      (setf tests
		    (cond
		      ((str:starts-with? "Native configuration is " line)
		       (setf host (str:substring 24 nil line))
		       tests)
		      ((str:starts-with? "Host   is" line)
		       (setf host (str:substring 10 nil line))
		       tests)
		      ((str:starts-with? "PASS:" line)
		       (cons
			(json:decode-json-from-string
			 (format nil "{ \"report\": \"dejagnu\", \"result\": \"PASS\", \"host\": \"~A\", \"id\": \"~A\" }"
				 host (str:substring 6 nil line)))
			tests))
		      ((str:starts-with? "FAIL:" line)
		       (cons
			(json:decode-json-from-string
			 (format nil "{ \"report\": \"dejagnu\", \"result\": \"FAIL\", \"host\": \"~A\", \"id\": \"~A\" }"
				 host (str:substring 6 nil line)))
			tests))
		      ((str:starts-with? "XFAIL:" line)
		       (cons
			(json:decode-json-from-string
			 (format nil "{ \"report\": \"dejagnu\", \"result\": \"XFAIL\", \"host\": \"~A\", \"id\": \"~A\" }"
				 host (str:substring 7 nil line)))
			tests))
		      ((str:starts-with? "XPASS:" line)
		       (cons
			(json:decode-json-from-string
			 (format nil "{ \"report\": \"dejagnu\", \"result\": \"XPASS\", \"host\": \"~A\", \"id\": \"~A\" }"
				 host (str:substring 7 nil line)))
			tests))
		      (t tests)))))
    tests))
