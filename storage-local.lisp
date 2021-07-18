;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019, 2020, 2021  Anthony Green <green@moxielogic.com>
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

(defclass storage/local (storage-backend)
  ((local-dir :reader local-dir)))

(defmethod initialize-instance :after ((backend storage/local) &key)
  "Initialize a local storage backend."
  (log:info "Initializing storage/local")
  (setf (slot-value backend 'local-dir)
        (or (gethash "local-dir" (config backend) "/var/rlgl/docs")))
  (let ((filename (str:concat (local-dir backend) "/.key" )))
    (if (probe-file filename)
	(setf (slot-value backend 'key)
	      (alexandria:read-file-into-string filename :external-format :latin-1))
	(let ((key (rlgl-util:random-hex-string)))
	  (with-open-file (stream filename
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
	    (format stream key)
	    (setf (slot-value backend 'key) key))))))

(defmethod delete-document ((backend storage/local) ref)
  "Delete a document from local storage."
  (delete-file (str:concat (local-dir backend) "/" ref)))

(defmethod read-document ((backend storage/local) ref)
  "Return an octet vector containing the document."
  (alexandria:read-file-into-byte-vector (str:concat (local-dir backend) "/" ref)))

(defmethod store-document ((backend storage/local) document)
  "Store a document into local storage."
  (let ((filename (concatenate 'string "RLGL-" (rlgl-util:random-hex-string))))
    (with-open-file (stream (str:concat (local-dir backend) "/" filename)
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (write-sequence document stream)
      filename)))
