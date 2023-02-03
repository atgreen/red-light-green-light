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

;;; MVP CSV parser

;; ----------------------------------------------------------------------------

(defclass parser/csv (report-parser)
  ()
  (:default-initargs
   :title  "CSV Report"
   :doctype "text"))

(defmethod parse-report ((parser parser/csv) doc labels)
  "Parse CSV content where the first row specifies the field names."
  (let* ((csv (cl-csv:read-csv doc
			       :trim-outer-whitespace t))
	 (fields (make-array (length (car csv))
			     :initial-contents (car csv))))
    (loop for row in (cdr csv)
	  ;; Filter out blank lines
	  unless (and (= (length row) 1)
		    (equalp "" (car row)))
       collect (json:decode-json-from-string
		  (let ((l (loop for i from 0 for e in row collect (list (aref fields i) e))))
		    (format nil "{ ~{\"~A\": \"~A\"~^, ~} ~A}" (alexandria:flatten l) (rlgl-util:jsonify-labels labels)))))))
