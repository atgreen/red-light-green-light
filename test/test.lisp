;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2018-2022  Anthony Green <green@moxielogic.com>
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
(setf prove:*debug-on-error* t)

(defvar *upload-ref* nil)

(defun test-eval (report)

  (print (pathname report))
  (subtest (format nil "upload ~A" report)
	   (let* ((params `((file . (,(pathname report)
                                     :content-type "application/octet-stream"
                                     :filename ,(pathname report)))))
                  (upload-ref
		    (drakma:http-request "http://localhost:8080/upload"
					 :method :post
                                         :form-data t
                                         :parameters params)))
             (setf *upload-ref* upload-ref)
	     (like upload-ref "RLGL-[A-Z0-9]+")))

  (subtest (format nil "evaluate ~A" report)
           (let* ((result (drakma:http-request "http://localhost:8080/evaluate"
				               :method :post
				               :content-type "application/json"
				               :content (format nil "{ \"id\": \"~A\", \"policy\": \"~A\", \"ref\": \"~A\" }"
						                (rlgl-util:random-hex-string)
						                "http://github.com/moxielogic/rlgl-toolchain-policy"
						                *upload-ref*)))
                  (json (json:decode-json-from-string result)))
             (setf *json* json)
             (like (cdr (assoc :CALLBACK json)) "[A-Z0-9]+[A-Z0-9]+[A-Z0-9]+[A-Z0-9]+[A-Z0-9]+[A-Z0-9]+[A-Z0-9]+[A-Z0-9]+")))

  (subtest (format nil "callback ~A" report)
           (let ((result (nth-value 1 (drakma:http-request "http://localhost:8080/callback"
				                           :method :post
				                           :content-type "application/json"
				                           :content (format nil "{ \"id\": \"~A\", \"signature\": \"MGUCMEYUlcrqJlgR+p8AKNP1hOTZqspNbOnXMssK3xDq2q0Z9J/y0owNCaNz5gWu/NTjMAIxAPpztPFIbjtWugP0cyqhe6L3mtrzUjZazLEcTOnThJmEWPni1COga+wIUtqvgN3VxQ==\" }"
						                            (cdr (assoc :CALLBACK *json*)))))))
             (is result 200)))
  )


(defun run ()

  (plan 1)

  (start-rlgl-server t nil "test/config.ini")

  ;; ---------------------------------------------------------------------------
  ;; API tests
  ;; ---------------------------------------------------------------------------

  (subtest "start test"
	   (loop for i from 0 to 10
	      do
		(let ((id (drakma:http-request "http://localhost:8080/start")))
		  (is (length id) 8))))

  (test-eval "test/report.html")
  (test-eval "test/sample-junit.xml")
  (test-eval "test/mysql-aqua.html")
  (test-eval "test/clair-report.json")
  (test-eval "test/empty-clair-report.json")

  (let* ((result (nth-value 1 (drakma:http-request "http://localhost:8080/evaluate"
				                   :method :post
				                   :content-type "application/json"
				                   :content (format nil "{ \"id\": \"~A\", \"policy\": \"http://github.com/atgreen/red-light-green-light\", \"ref\": \"~A\" }"
						                    (rlgl-util:random-hex-string)
                                                                    *upload-ref*)))))
    (log:info result))

  (finalize)

  (if (uiop:getenv "RLGL_WAIT4EVER")
      (progn
	(format t "Waiting forever.  Hit CTRL-C to exit.")
	(loop (sleep 3000))))

  )
