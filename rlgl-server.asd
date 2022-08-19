;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2018-2022  Anthony Green <green@moxielogic.com>
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

(asdf:defsystem #:rlgl-server
  :description "The Red Light Green Light server."
  :author "Anthony Green <green@moxielogic.com>"
  :version "2"
  :serial t
  :components ((:file "package")
	       (:file "api-key")
	       (:file "matcher")
	       (:file "policy")
	       (:file "storage-local")
	       (:file "storage-s3")
	       (:file "rlgl-server"))
  :depends-on (:rlgl-parsers :rlgl-util :rlgl-db :rlgl-user
               :metabang-bind :markup :cl-template
               :cl-toml :snooze :cl-json :plump :lquery :ironclad
	       :inferior-shell :hunchentoot :spinneret
	       :cl-json-util :cl-fad :str :log4cl :cl-ppcre
               :cxml :cl-dbi :cl-date-time-parser :quri
	       :local-time :cl-csv :prometheus
	       :dbd-sqlite3 :zs3 :simple-date-time
	       :drakma :uuid :cl-base32
	       :split-sequence :thread-pool
	       :prometheus.formats.text
	       :prometheus.exposers.hunchentoot
	       :prometheus.collectors.sbcl
	       :prometheus.collectors.process))
