;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019, 2020, 2021, 2023  Anthony Green <green@moxielogic.com>
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

(defpackage #:rlgl-util
  (:use #:cl)
  (:shadow #:package)
  (:export #:random-base36-string
	   #:valid-url?
	   #:make-absolute-pathname
           #:jsonify-labels
	   #:escape-json-string))

(in-package #:rlgl-util)

(defvar *random-state-lock*
  (bt:make-lock "Random state lock"))
(setf *random-state* (make-random-state t))

(defun random-base36-string ()
  "Return a random base36 (0-9A-Z) string of 8 characters."
  (bt:with-lock-held (*random-state-lock*)
    (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*))))

(defparameter +root-path+ (asdf:component-pathname (asdf:find-system "rlgl-server")))

(defun make-absolute-pathname (pathname)
  "Return an absolute pathname.  If PATHNAME is relative, make it
   relative to the rlgl-server system (provided by asdf)."
  (if (cl-fad:pathname-absolute-p pathname)
      pathname
      (merge-pathnames +root-path+ pathname)))

(defun valid-url? (string)
  "Returns T if STRING is a valid http or https url."
  (and string
       (quri:uri-http-p (quri:uri string))))

(defun escape-json-string (string)
  "Escape character sequences used for json strings."
  (let* ((s1 (str:replace-all "\\" "\\\\\\\\" string))
	 (s2 (str:replace-all "\"" "\\\\\\\"" s1)))
    (format nil "~A~%" s2)
    s2))

(defun jsonify-labels (labels)
  "Convert a list of LABELS to a JSON string.

Each label is a key-value pair. The key is converted to CamelCase
using the `json:lisp-to-camel-case` function.

Example: (jsonify-labels '((foo . 1) (bar . 2))) => ', \"foo\": 1, \"bar\": 2'"
      (flet ((pair-to-json-field-string (key-value-pair)
               (format nil "~S: ~S"
                       (json:lisp-to-camel-case (symbol-name (car key-value-pair)))
                       (cdr key-value-pair))))
        (if labels
            (format nil ", ~{~A~^, ~}"
                    (mapcar #'pair-to-json-field-string labels))
            "")))
