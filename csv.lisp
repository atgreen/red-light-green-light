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

(in-package :rlgl-server)

;;; MVP CSV parser

;; ----------------------------------------------------------------------------

(defclass parser/csv (report-parser)
  ()
  (:default-initargs
   :title  "CSV Report"))

(defmethod parse-report ((parser parser/csv) doc)
  "Parse CSV content where the first row specifies the field names."
  (let* ((csv (cl-csv:read-csv doc))
	 (fields (make-array (length (car csv))
			     :initial-contents (car csv))))
    (mapcar
     (lambda (row)
       (json:decode-json-from-string
	(let ((l (loop for i from 0 for e in row collect (list (aref fields i) e))))
	  (format nil "{ ~{\"~A\": \"~A\"~^, ~} }" (alexandria:flatten l)))))
     (cdr csv))))
