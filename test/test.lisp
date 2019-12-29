;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
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

(in-package #:cl-user)
(defpackage #:test-rlgl-server
  (:use #:common-lisp #:rlgl-server #:prove #:matcher #:policy)
  (:export #:run))
(in-package #:test-rlgl-server)

(setf prove:*default-reporter* :fiveam)

(defun test-eval (report)
  
    (subtest (format nil "upload ~A" report)
	     (let ((upload-ref
		    (drakma:http-request "http://localhost:8080/upload"
					 :method :post
					 :content-type "application/octet-stream"
					 :content (pathname report))))
	       (like upload-ref "RLGL-[A-Z0-9]+")
	       (setf *upload-ref* upload-ref)))
  
    (subtest (format nil "evaluate ~A" report)
	     (print
	      (drakma:http-request "http://localhost:8080/evaluate"
				   :method :post
				   :content-type "application/json"
				   :content (format nil "{ \"id\": \"~A\", \"policy\": \"~A\", \"ref\": \"~A\" }"
						    (rlgl.util:random-hex-string 7)
						    "http://github.com/moxielogic/rlgl-toolchain-policy"
						    *upload-ref*))))
    )


(defun run ()

  (plan 1)
  
  (start-rlgl-server nil)

  ;; ---------------------------------------------------------------------------
  ;; API tests
  ;; ---------------------------------------------------------------------------

  (subtest "start test"
	   (loop for i from 0 to 10
	      do
		(let ((id (drakma:http-request "http://localhost:8080/start")))
		  (is (length id) 7))))
  
  (defvar *upload-ref* nil)

  (test-eval "test/report.html") 
  (test-eval "test/sample-junit.xml")
  (test-eval "test/mysql-aqua.html")
  (test-eval "test/clair-report.json")
  (test-eval "test/empty-clair-report.json")
  (test-eval "test/gcc2.sum")

  (finalize)

  (if (uiop:getenv "RLGL_WAIT4EVER")
      (progn
	(format t "Waiting forever.  Hit CTRL-C to exit.")
	(loop (sleep 3000))))

  )
