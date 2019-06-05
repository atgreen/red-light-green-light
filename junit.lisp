;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
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

;;; MVP JUnit XML parser

;; ----------------------------------------------------------------------------

(defclass parser/junit (report-parser)
  ()
  (:default-initargs
   :title  "JUnit Test Report"))

(defmethod parse-report ((parser parser/junit) doc)
  (let ((xmls (cxml:parse-octets (flexi-streams:string-to-octets doc)
				(cxml-xmls:make-xmls-builder)))
	(tests-fail (list))
	(tests-pass (list)))
    (let ((children (cdr (cdr xmls))))
      (remove nil
	      (mapcar
	       (lambda (child)
		 (if (and (listp child)
			  (string= (car child) "testcase"))
		     (let ((classname nil)
			   (result nil))
		       (dolist (a (car (cdr child)))
			 (cond
			   ((string= (car a) "classname")
			    (setf classname (car (cdr a))))
			   ((string= (car a) "name")
			    (setf result (car (cdr a))))))
		       (json:decode-json-from-string
			(format nil "{ \"report\": \"junit\", \"result\": \"~A\", \"id\": \"~A\" }"
				result classname)))
		     nil))
	       children)))))
