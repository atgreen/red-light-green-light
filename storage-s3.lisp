;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2019  Anthony Green <green@moxielogic.com>
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

(in-package :rlgl-server)

;;; MVP storage driver using s3 storage

;; ----------------------------------------------------------------------------
;; Make an AWS credentials list from environment variables

(defun getenv-aws-credentials ()
  (list (uiop:getenv "AWS_ACCESS_KEY")
	(uiop:getenv "AWS_SECRET_KEY")))

;; ----------------------------------------------------------------------------
;; Utility functions for random filenames...

(defvar *name-random-state* (make-random-state t))
(defvar *create-file-name-lock*
  (bordeaux-threads:make-lock "Temporary File Name Creation Lock"))

(defmacro with-file-name-lock-held (() &body body)
  `(bordeaux-threads:with-lock-held (*create-file-name-lock*)
     ,@body))

(defun generate-random-string ()
  (with-file-name-lock-held ()
    (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) *name-random-state*))))

;; ----------------------------------------------------------------------------

(defclass storage/s3 (storage-backend)
  ((s3-endpoint       :initarg :s3-endpoint    :reader s3-endpoint)
   (s3-bucket         :initarg :s3-bucket      :reader s3-bucket))
  (:default-initargs
   :s3-endpoint  "s3.amazonaws.com"
   :s3-bucket    "rlgl-docs-2"))

(defmethod initialize-instance :after ((backend storage/s3) &key)
  "Initialize a s3 storage backend."
  (log:info "Initializing storage/s3")
  (setf zs3:*credentials* (getenv-aws-credentials))
  (unless (zs3:bucket-exists-p (s3-bucket backend))
    (zs3:create-bucket (s3-bucket backend))))

(defmethod read-document ((backend storage/s3) ref)
  "Return a string containing the document."
  (zs3:get-vector (s3-bucket backend)
		  (format nil "~A" ref)))

(defmethod store-document ((backend storage/s3) document)
  "Store a document into s3 storage."
  (let ((ref (format nil "RLGL-~A"
		     (generate-random-string))))
    (zs3:put-vector document (s3-bucket backend) ref)
    ref))
