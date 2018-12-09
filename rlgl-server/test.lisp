(in-package :cl-user)

(ql:quickload :drakma)

(defpackage rlgl-server-test
  (:use :cl
	:rlgl-server
	:drakma
	:prove))
(in-package :rlgl-server-test)

(subtest "Example test"
  (start-rlgl-server nil)

  (is (drakma:http-request "http://localhost:8081/start") 5556)
  (is (drakma:http-request "http://localhost:8081/start") 5557)
  (is (drakma:http-request "http://localhost:8081/start") 5558)
  (is (drakma:http-request "http://localhost:8081/start") 5559)

  (stop-rlgl-server))

(finalize)

