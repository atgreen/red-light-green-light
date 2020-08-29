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

(in-package :rlgl-server)

;;; MVP storage driver using local storage

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

(defclass storage/local (storage-backend)
  ((local-dir       :initarg :local-dir    :reader local-dir))
  (:default-initargs
   :local-dir  "/var/rlgl/docs"))

(defmethod initialize-instance :after ((backend storage/local) &key)
  "Initialize a local storage backend."
  (log:info "Initializing storage/local")
  (let ((filename (format nil "~A/.key" (local-dir backend))))
    (if (probe-file filename)
	(setf (slot-value backend 'key)
	      (alexandria:read-file-into-string filename :external-format :latin-1))
	(let ((key (generate-random-string)))
	  (with-open-file (stream filename
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
	    (format stream key)
	    (setf (slot-value backend 'key) key))))))

(defmethod read-document ((backend storage/local) ref)
  "Return an octet vector containing the document."
  (alexandria:read-file-into-byte-vector (format nil "~A/~A" (local-dir backend) ref)))

(defmethod store-document ((backend storage/local) document)
  "Store a document into local storage."
  (let ((filename (format nil "RLGL-~A"
			  (generate-random-string))))
    (with-open-file (stream (format nil "~A/~A" (local-dir backend) filename)
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (write-sequence document stream)
      filename)))
