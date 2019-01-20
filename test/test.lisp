(in-package #:cl-user)
(defpackage #:test-rlgl-server
  (:use #:common-lisp #:rlgl-server #:prove #:matcher #:policy)
  (:export #:run))
(in-package #:test-rlgl-server)

(setf prove:*default-reporter* :fiveam)

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
  
  (subtest "upload test"
	   (let ((upload-ref
		  (drakma:http-request "http://localhost:8080/upload"
				       :method :post
				       :content-type "application/octet-stream"
				       :content #p"test/report.html")))
	     (like upload-ref "RLGL-[A-Z0-9]+")
	     (setf *upload-ref* upload-ref)))
  
  (subtest "evaluate test"
	   (print
	    (drakma:http-request "http://localhost:8080/evaluate"
				 :method :post
				 :content-type "application/json"
				 :content (format nil "{ \"ref\": \"~A\" }" *upload-ref*))))
  
  (subtest "upload test"
	   (let ((upload-ref
		  (drakma:http-request "http://localhost:8080/upload"
				       :method :post
				       :content-type "application/octet-stream"
				       :content #p"test/sample-junit.xml")))
	     (like upload-ref "RLGL-[A-Z0-9]+")
	     (setf *upload-ref* upload-ref)))
  
  (subtest "evaluate test"
	   (print
	    (drakma:http-request "http://localhost:8080/evaluate"
				 :method :post
				 :content-type "application/json"
				 :content (format nil "{ \"ref\": \"~A\" }" *upload-ref*))))

  (finalize)

  (if (uiop:getenv "RLGL_WAIT4EVER")
      (progn
	(format t "Waiting forever.  Hit CTRL-C to exit.")
	(loop (sleep 3000))))

  )
