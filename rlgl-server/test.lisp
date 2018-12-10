(in-package :cl-user)

(ql:quickload :drakma)

(defpackage rlgl-server-test
  (:use :cl
	:rlgl-server
	:drakma
	:prove))
(in-package :rlgl-server-test)

(setf prove:*default-reporter* :fiveam)

(plan 1)

(start-rlgl-server nil)

(subtest "start test"
  (loop for i from 5556 to 5566
     do
       (is (drakma:http-request "http://localhost:8081/start") (format nil "~A" i))))

(defvar *upload-ref* nil)

(subtest "upload test"
  (let ((upload-ref
	 (drakma:http-request "http://localhost:8081/upload"
			      :method :post
			      :content-type "application/octet-stream"
			      :content #p"./test.lisp")))
    (like upload-ref "rlgl-[A-Z0-9]+")
    (setf *upload-ref* upload-ref)))
    
(subtest "evaluate test"
  (print
   (drakma:http-request "http://localhost:8081/evaluate"
			:method :post
			:content-type "application/json"
			:content (format nil "{ \"ref\": \"~A\" }" *upload-ref*))))

(finalize)


