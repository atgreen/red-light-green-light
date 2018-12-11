;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018  Anthony Green <green@moxielogic.com>
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

(defpackage #:policy
  (:use #:cl #:matcher #:cl-fad)
  (:shadow #:package)
  (:export #:make-policy))

(in-package #:policy)

(defvar *git-log-table* (make-hash-table :test 'equal))

(defclass policy ()
  ((xfail-matchers :reader xfail-matchers)
   (pass-matchers  :reader pass-matchers)
   (fail-matchers  :reader fail-matchers))
  )

(defun make-policy (directory)
  (let ((xfail-file (merge-pathnames-as-file directory #p"XFAIL"))
	(pass-file (merge-pathnames-as-file directory #p"PASS"))
	(fail-file (merge-pathnames-as-file directory #p"FAIL")))

    (mapc (lambda (file)
	    (if (not (file-exists-p file))
		(error (format nil "Policy file \"~A\" missing." file))))
	  (list xfail-file pass-file fail-file))
    
    (let ((p (make-instance 'policy)))
      (setf (slot-value p 'xfail-matchers) (read-json-patterns xfail-file))
      (setf (slot-value p 'pass-matchers) (read-json-patterns pass-file))
      (setf (slot-value p 'fail-matchers) (read-json-patterns fail-file))
      p)))

(defun read-json-patterns (filename)
  (let ((patterns (list)))
    (let ((matcher-lines (inferior-shell:run/lines
			  (format nil "git blame -s -l ~A" filename))))
      (mapc (lambda (matcher-line)
	      (let ((githash (subseq matcher-line 0 40)))
		(multiple-value-bind (lineno location)
		    (read-from-string (subseq matcher-line 40))
		  (let ((line (string-trim '(#\Space #\Tab)
					   (subseq matcher-line (+ 41 location)))))
		    (if (and (> (length line) 0)
			     (null (find (char line 0) "#;-")))
			(let ((json (json:decode-json-from-string line)))
			  (setf patterns (cons (make-policy-matcher :githash githash
								    :lineno lineno
								    :matcher json)
					       patterns))))))))
	    matcher-lines)

      ;; Now go through git logs
      (mapc (lambda (matcher)
	      (let ((log-entry (gethash (githash matcher) *git-log-table*)))
		(if (null log-entry)
		    (progn
		      (setf log-entry (inferior-shell:run/lines
				       (format nil "git log -r ~A ~A"
					       (githash matcher) filename)))
		      (setf (gethash (githash matcher) *git-log-table*) log-entry)))
		(setf (slot-value matcher 'log-entry) log-entry)))
	    patterns)

      patterns)))

