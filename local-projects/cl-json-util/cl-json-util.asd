(asdf:defsystem :cl-json-util
  :version "0.0.1"
  :description "JSON utilities for Common Lisp"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :depends-on (:jsown)
  :components ((:static-file "cl-json-util.asd")
               (:file "cl-json-util")))
