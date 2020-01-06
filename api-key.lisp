;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019, 2020  Anthony Green <green@moxielogic.com>
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

;; Make base32-style API keys.
;; Inspired by https://www.npmjs.com/package/uuid-apikey

(defpackage #:rlgl.api-key
  (:use #:cl)
  (:shadow #:package)
  (:export #:make-api-key #:authorize-by-api-key))

(in-package #:rlgl.api-key)

(defun authorize-by-api-key (db api-key)
  (if (rlgl.db:find-puk-by-api-key db api-key)
      t
      nil))

(defun int-to-byte-array (int)
  (let ((a (make-array 4)))
    (setf (aref a 0) (ldb (byte 8 0) int))
    (setf (aref a 1) (ldb (byte 8 8) int))
    (setf (aref a 2) (ldb (byte 8 16) int))
    (setf (aref a 3) (ldb (byte 8 24) int))
    a))

(defun string-to-base32 (s start end)
  (str:substring
   0 7 
   (base32:bytes-to-base32
    (int-to-byte-array (parse-integer
			(str:substring start end s) :radix 16)))))

(defun make-api-key ()
  "Make a base32-style API key."
  (let ((uuid (str:replace-all "-" ""
			       (print-object (uuid:make-v4-uuid) nil))))
    (str:upcase
     (str:concat
      (string-to-base32 uuid 0 7)
      "-"
      (string-to-base32 uuid 8 15)
      "-"
      (string-to-base32 uuid 16 23)
      "-"
      (string-to-base32 uuid 24 31)))))
