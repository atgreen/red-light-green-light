;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019  Anthony Green <green@moxielogic.com>
;;;                         
;;; Rlgl-Server is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; Rlgl-Server is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Rlgl-Server; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

(asdf:defsystem #:test-rlgl-server
  :description "Test the Red Light Green Light server."
  :depends-on (:rlgl-server :prove :drakma)
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "test")))))

(defmethod perform ((o test-op) (c (eql (find-system :test-rlgl-server))))
  (funcall (intern (string '#:run) '#:test-rlgl-server)))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :test-rlgl-server))))
  nil)
