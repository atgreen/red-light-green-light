;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-PARSERS; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019, 2020, 2021, 2023  Anthony Green <green@moxielogic.com>
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

;;; MVP OpenScap OVAL Results report parser

;; ----------------------------------------------------------------------------

(defclass parser/oscap-oval (report-parser)
  ()
  (:default-initargs
   :title  "OpenSCAP OVAL Scan Report"
   :doctype "html"))

(defmethod parse-report ((parser parser/oscap-oval) doc labels)
  (let ((pdoc (plump:parse (flexi-streams:make-flexi-stream
			    (flexi-streams:make-in-memory-input-stream doc)
			    :external-format :utf-8)))
	(tests-fail (list))
	(tests-pass (list)))
;;; Extract date
;;;    (lquery:$ pdoc "body > table.noborder.nomargin > tbody > tr > td:nth-child(2) > table > tbody > tr:nth-child(3) > td:nth-child(4)"
;;;	      text
    (lquery:$ pdoc "tr.resultbadA > td:nth-child(4) > a"
	      (combine (attr :href) (text))
	      (map-apply #'(lambda (url text)
			     (setf tests-fail
				   (cons
				    (json:decode-json-from-string
				     (format nil "{ \"report\": \"oscap-oval\", \"result\": \"FAIL\", \"id\": \"~A\", \"url\": \"~A\" ~A}"
					     text url (rlgl-util:jsonify-labels labels)))
				    tests-fail)))))
    (lquery:$ pdoc "tr.resultgoodA > td:nth-child(4) > a"
	      (combine (attr :href) (text))
	      (map-apply #'(lambda (url text)
			     (setf tests-pass
				   (cons
				    (json:decode-json-from-string
				     (format nil "{ \"report\": \"oscap-oval\", \"result\": \"PASS\", \"id\": \"~A\", \"url\": \"~A\" ~A}"
					     text url (rlgl-util:jsonify-labels labels)))
				    tests-pass)))))
    (append tests-fail tests-pass)))
