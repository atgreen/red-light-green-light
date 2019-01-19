;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019  Anthony Green <green@moxielogic.com>
;;;                         
;;; rlgl-server is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; rlgl-server is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rlgl-server; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

;; Matcher routines

(defpackage #:matcher
  (:use #:cl)
  (:shadow #:package)
  (:export #:make-policy-matcher #:matcher #:githash #:log-entry
	   #:kind
	   #:match-pair-in-alist #:match-candidate-pattern))

(in-package #:matcher)

(defclass policy-matcher ()
  ((kind    :initarg :kind    :reader kind)
   (githash :initarg :githash :reader githash)
   (lineno  :initarg :lineno  :reader lineno)
   (matcher :initarg :matcher :reader matcher)
   (log-entry :reader log-entry)))

(defun make-policy-matcher (&key kind (githash nil)
			      (lineno 0)
			      (matcher nil))
  (make-instance 'policy-matcher :kind kind :githash githash :lineno lineno :matcher matcher))

(defun match-pair-in-alist (pair alist)
  "Given a cons PAIR, return non-NIL if that PAIR matches
in ALIST, where a match means the CDRs are EQUALP."
  (let ((c (assoc (car pair) alist)))
    (and c (equalp (cdr pair) (cdr c)))))

(defun match-candidate-pattern (candidate pattern)
  "Given a CANDIDATE alist, return T if PATTERN matches CANDIDATE."
  (not (find-if-not (lambda (v)
		      (match-pair-in-alist v candidate))
		    pattern)))

