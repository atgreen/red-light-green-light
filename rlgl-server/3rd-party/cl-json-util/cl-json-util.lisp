(defpackage cl-json-util
  (:use :cl)
  (:nicknames :json-util)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export #:pretty-json
           #:pprint-json))

(in-package :cl-json-util)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))


(defun pretty-json (json &key (level 0))
  (if (stringp json)
      (pretty-json (jsown:parse json))
      (if (eq :obj (car json))
          ;; json is an object
          (format nil (format nil "{~~%~~{~~A~~^,~~%~~}~~%~~~A@A}" (* level 4))
                  (loop for (key . value) in (cdr json)
                     collect (format nil
                                     (format nil "~~~A@A\"~~A\": ~~:[~~S~~;~~A~~]" (* (1+ level) 4))
                                     ""
                                     key
                                     (listp value)
                                     (if (listp value)
                                         (pretty-json value :level (1+ level))
                                         value)))
                  "")
          ;; json ia an array
          (if (eq (length json) 0)
              "[]"
              (format nil (format nil "[~~%~~~A@A~~{~~A~~^,~~%~A~~}~~%~~~A@A]"
                                  (* (1+ level) 4)
                                  (format nil (format nil "~~~A@A" (* (1+ level) 4))
                                          "")
                                  (* level 4))
                      ""
                      (loop for value in json
                         collect (format nil
                                         (format nil "~~~A@A~~A" (* (1+ level) 4))
                                         (if (listp value)
                                             (pretty-json value :level (1+ level))
                                             value)
                                         ""))
                      "")))))

(defun pprint-json (json)
  (format t "~A~%" (pretty-json json))
  (values))
