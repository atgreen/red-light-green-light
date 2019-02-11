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
						    "http://github.com/atgreen/test-policy"
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

  (finalize)

  (if (uiop:getenv "RLGL_WAIT4EVER")
      (progn
	(format t "Waiting forever.  Hit CTRL-C to exit.")
	(loop (sleep 3000))))

  )
