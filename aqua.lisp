;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019  Anthony Green <green@moxielogic.com>
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

;;; MVP Aqua report parser

;; ----------------------------------------------------------------------------

(defclass parser/aqua (report-parser)
  ()
  (:default-initargs
   :title  "Aqua Scan Report"))

(defmethod parse-report ((parser parser/aqua) doc)
  (let ((pdoc (plump:parse (flexi-streams:make-flexi-stream
			    (flexi-streams:make-in-memory-input-stream doc)
			    :external-format :utf-8)))
	(tests-fail (list)))

    (let* ((vulns (lquery:$ pdoc "#cves > tbody > tr > tr > td:nth-child(1) > a"
     			    (combine (attr :href) (text))))
	   (severity (lquery:$ pdoc "#cves > tbody > tr > tr > td:nth-child(3) > span"
			       (text)))
	   (score (lquery:$ pdoc "#cves > tbody > tr > tr > td:nth-child(4) > span"
			    (text))))
      
      (loop for i from 0 to (- (length vulns) 1)
	 do
	   (progn
	     (setf tests-fail
		   (cons
		    (json:decode-json-from-string
		     (format nil
			     "{ \"report\": \"aqua\", \"result\": \"FAIL\", \"id\": \"~A\", \"url\": \"~A\", \"severity\": \"~A\", \"score\": \"~A\" }"
			     (car (cdr (aref vulns i)))
			     (car (aref vulns i))
			     (aref severity i)
			     (aref score i)))
		    tests-fail)))))

    tests-fail))

  
