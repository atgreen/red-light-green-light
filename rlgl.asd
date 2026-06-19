;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2018-2025  Anthony Green <green@moxielogic.com>
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

(asdf:defsystem #:rlgl
  :description "Red Light Green Light - a client-side policy enforcement tool."
  :author "Anthony Green <green@moxielogic.com>"
  :version "2.0.2"
  :serial t
  :components ((:file "package")
               (:file "matcher")
               (:file "policy")
               (:file "rlgl"))
  :depends-on (:rlgl-parsers :rlgl-util
               :alexandria :bordeaux-threads :clingon :markup
               :cl-json :jsown-utils :cl-fad :str :log4cl :cl-ppcre
               :ironclad :flexi-streams :inferior-shell :metabang-bind
               :cl-date-time-parser :quri :version-string)
  :build-operation "program-op"
  :build-pathname "rlgl"
  :entry-point "rlgl:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
