;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>
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

(asdf:defsystem #:rlgl-parsers
  :description "Parsers for rlgl"
  :author "Anthony Green <green@moxielogic.com>"
  :version "0"
  :serial t
  :components ((:file "package")
               (:file "report-parser")
               (:file "anchore")
               (:file "aqua")
               (:file "clair")
               (:file "csv")
               (:file "dejagnu")
               (:file "junit")
               (:file "oscap-oval")
               (:file "oscap-xccdf")
               (:file "tripwire-pdf"))
  :depends-on (:cl-json :plump :lquery :flexi-streams
               :str :cxml :quri :cl-csv :rlgl-util :inferior-shell))
