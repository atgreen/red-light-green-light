(in-package :cl-user)

(ql:quickload :drakma)

(defpackage rlgl-server-test
  (:use :cl
	:rlgl-server
	:matcher
	:policy
	:drakma
	:prove))
(in-package :rlgl-server-test)

(setf prove:*default-reporter* :fiveam)

(plan 1)

(start-rlgl-server nil)

;; -----------------------------------------------------------------------------
;; matcher tests
;; -----------------------------------------------------------------------------

(let ((a '((:a . "1") (:b . "2") (:c . "3")))
      (b '(:c . "3"))
      (c '(:c . "4"))
      (d '(:d . "5")))
  
  (subtest "match-pair-in-alist"
    (ok (not (match-pair-in-alist d a)))
    (ok (not (match-pair-in-alist c a)))
    (ok (match-pair-in-alist b a)))

  (subtest "match-candidate-pattern"
    (ok (match-candidate-pattern a a))
    (ok (match-candidate-pattern a (list b)))
    (ok (not (match-candidate-pattern a (list b c)))))
  )

;; -----------------------------------------------------------------------------
;; policy tests
;; -----------------------------------------------------------------------------

;(subtest "policy"
;  (ok (make-policy "https://gogs-labdroid.apps.home.labdroid.net/green/test-policy.git")))

;; -----------------------------------------------------------------------------
;; API tests
;; -----------------------------------------------------------------------------

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
			      :content #p"../test/report.html")))
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
