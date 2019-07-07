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
;; Credentials object to pull AWS credentials from environment variables.

(defclass environment-credentials () ())

(defmethod access-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (getenv "AWS_ACCESS_KEY"))

(defmethod secret-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (getenv "AWS_SECRET_KEY"))

(setf *credentials* (make-instance 'environment-credentials))

;; The rest of this file is a big TODO...

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

(defclass s3-storage-backend (storage-backend)
  ((s3-endpoint       :initarg :s3-endpoint    :reader s3-endpoint)
   (s3-bucket         :initarg :s3-bucket      :reader s3-bucket))
  (:default-initargs
   :s3-endpoint  "s3.amazonaws.com"))

(defmethod init ((backend s3-storage-backend))
  "Initialize a s3 storage backend."
  (unless (zs3:bucket-exists-p (s3-bucket s3-storage-backend))
    (zs3:create-bucket (s3-bucket s3-storage-backend))))

(defmethod read-document ((backend s3-storage-backend) ref)
  "Return a string containing the document.")

(defmethod store-document ((backend s3-storage-backend) document)
  "Store a document into s3 storage."
  (let ((filename (format nil "RLGL-~A"
			  (generate-random-string))))
    (put-vector document (s3-bucket s3-storage-backend) filename)
    filename))
