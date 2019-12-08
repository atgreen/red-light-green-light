;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
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

(defpackage #:rlgl.util
  (:use #:cl)
  (:shadow #:package)
  (:export #:random-hex-string
	   #:valid-url?
	   #:read-file-into-string
	   #:escape-json-string))

(in-package #:rlgl.util)

(setf *random-state* (make-random-state t))

(defun random-hex-string (&optional (length 7))
  (let ((chars "abcdef0123456789"))
    (coerce (loop repeat length collect (aref chars (random (length chars))))
            'string)))

(defparameter +root-path+ (asdf:component-pathname (asdf:find-system "rlgl-server")))

(defun read-file-into-string (filename)
  "Read FILENAME into a string and return that.
   If filename is not an absolute path, find it relative to the
   rlgl-server system (provided by asdf)."
  (let ((absolute-filename (if (cl-fad:pathname-absolute-p filename)
			       filename
			       (merge-pathnames +root-path+ filename))))
    (with-open-file (stream absolute-filename :external-format :latin-1)
      (let ((contents (make-string (file-length stream))))
	(read-sequence contents stream)
	contents))))

(defun valid-url? (string)
  "Returns T if STRING is a valid http or https url."
  (and string
       (quri:uri-http-p (quri:uri string))))

(defun escape-json-string (string)
  "Escape character sequences used for json strings."
  (let* ((s1 (str:replace-all "\\" "\\\\\\" string))
	 (s2 (str:replace-all "\"" "\\\\\"" s1)))
    (format nil "~A~%" s2)
    s2))
