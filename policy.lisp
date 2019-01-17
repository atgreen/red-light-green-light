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

(defpackage #:policy
  (:use #:cl #:matcher #:cl-fad)
  (:shadow #:package)
  (:export #:make-policy #:apply-policy #:commit-url-format))

(in-package #:policy)

(defvar *git-log-table* (make-hash-table :test 'equal))

(defclass policy ()
  ((xfail-matchers :reader xfail-matchers)
   (pass-matchers  :reader pass-matchers)
   (fail-matchers  :reader fail-matchers)
   (commit-url-format :reader commit-url-format))
  )

(defun guess-commit-url-format (url)
  "Guess the commit URL format string based on URL."
  ;; We should be able to guess github, gitlab and gogs commit URLs.
  ;; This is only ever called when we aren't handed a commit URL
  ;; in MAKE-POLICY.
  ;; FIXME: this is a hardcoded for now.  Write this function.
  "https://gogs-labdroid.apps.home.labdroid.net/green/test-policy/commit/~A")
  
(defun make-policy (url &key (commit-url-format (guess-commit-url-format url)))
  "Create an intance of a POLICY object based on the contents of the
git repo at URL.  If :COMMIT-URL-FORMAT is provided, use that as the
format string for generating a git commit url given a commit hash
argument.  If not provided, we will try to guess this format string
based on URL."

  (let ((output (inferior-shell:run (format nil "/usr/bin/git clone ~A" url))))
    (mapc (lambda (line)
	    (format t line))
	  output))

  (let ((xfail-file (merge-pathnames-as-file #p"test-policy/" #p"XFAIL"))
	(pass-file (merge-pathnames-as-file #p"test-policy/" #p"PASS"))
	(fail-file (merge-pathnames-as-file #p"test-policy/" #p"FAIL")))

    (mapc (lambda (file)
	    (if (not (file-exists-p file))
		(error (format nil "Policy file \"~A\" missing." file))))
	  (list xfail-file pass-file fail-file))
    
    (let ((p (make-instance 'policy)))
      (setf (slot-value p 'commit-url-format) commit-url-format)
      (setf (slot-value p 'xfail-matchers) (read-json-patterns :XFAIL xfail-file))
      (setf (slot-value p 'pass-matchers) (read-json-patterns :PASS pass-file))
      (setf (slot-value p 'fail-matchers) (read-json-patterns :FAIL fail-file))
      p)))

(defun read-json-patterns (kind filename)
  (let ((patterns (list)))
    (let ((matcher-lines (inferior-shell:run/lines
			  (format nil "bash -c \"(cd test-policy; git blame -s -l ~A)\"" "XFAIL"))))
      (mapc (lambda (matcher-line)
	      (let ((githash (subseq matcher-line 1 41)))
		(multiple-value-bind (lineno location)
		    (read-from-string (subseq matcher-line 40))
		  (let ((line (string-trim '(#\Space #\Tab)
					   (subseq matcher-line (+ 41 location)))))
		    (if (and (> (length line) 0)
			     (null (find (char line 0) "#;-")))
			(let ((json (json:decode-json-from-string line)))
			  (setf patterns (cons (make-policy-matcher :kind kind
								    :githash githash
								    :lineno lineno
								    :matcher json)
					       patterns))))))))
	    matcher-lines)

      ;; Now go through git logs
      (mapc (lambda (matcher)
	      (let* ((githash (githash matcher))
		     (log-entry (gethash githash *git-log-table*)))
		(if (and (null log-entry)
			 (not (string= githash ; check for local change
				       "0000000000000000000000000000000000000000")))
		    (progn
		      (setf log-entry (inferior-shell:run/lines
				       (format nil "bash -c \"(cd test-policy; git log -r ~A ~A)\""
					       githash "XFAIL")))
		      (setf (gethash githash *git-log-table*) log-entry)))
		(setf (slot-value matcher 'log-entry) log-entry)))
	    patterns)

      patterns)))

(defun apply-policy (policy candidate-result-list)
  (mapcar (lambda (result)
	    (cons
	     (or
	      (find-if (lambda (matcher)
			 (match-candidate-pattern result (matcher matcher)))
		       (xfail-matchers policy))
	      (find-if (lambda (matcher)
			 (match-candidate-pattern result (matcher matcher)))
		       (fail-matchers policy))
	      (find-if (lambda (matcher)
			 (match-candidate-pattern result (matcher matcher)))
		       (pass-matchers policy)))
	     result))
	  candidate-result-list))
