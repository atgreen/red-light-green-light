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

;;; MVP Aqua report parser

;; ----------------------------------------------------------------------------

(defclass parser/aqua (report-parser)
  ()
  (:default-initargs
   :title  "Aqua Scan Report"))

(defmethod parse-report ((parser parser/aqua) doc)
  (let ((pdoc (plump:parse doc))
	(tests-fail (list))
	(tests-pass (list)))
    (lquery:$ pdoc "#cves > tbody > tr > tr > td:nth-child(1) > a"
     	      (combine (attr :href) (text))
     	      (map-apply #'(lambda (url text)
     			     (setf tests-fail
     				   (cons
     				    (json:decode-json-from-string
     				     (format nil "{ \"report\": \"aqua\", \"result\": \"FAIL\", \"id\": \"~A\", \"url\": \"~A\" }"
     					     text url))
     				    tests-fail)))))
    (append tests-fail tests-pass)))
