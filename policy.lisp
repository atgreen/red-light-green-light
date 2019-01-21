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
  (:export #:*policy-dir* #:make-policy #:apply-policy #:commit-url-format
	   #:compile-scanners))

(in-package #:policy)

(defvar *policy-lock* (bt:make-lock))

(defvar *policy-dir* nil)

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

  ;; Hold one big lock, just in case...
  (bt:with-lock-held (*policy-lock*)

    (let* ((policy-dirname (str:concat (namestring *policy-dir*)
				       (subseq (ironclad:byte-array-to-hex-string
						(ironclad:digest-sequence
						 :sha1 (flexi-streams:string-to-octets url)))
					       0 8))))

      (when (fad:directory-exists-p policy-dirname)
	  (sb-ext:delete-directory policy-dirname :recursive t))
      
      (let ((output (inferior-shell:run
		     (format nil "/usr/bin/git clone ~A ~A"
			     url policy-dirname))))
	(mapc (lambda (line)
		(format t line))
	      output))

      (let ((policy-pathname
	     (fad:pathname-as-directory (make-pathname :name policy-dirname))))
	
	(let ((xfail-file (merge-pathnames-as-file policy-pathname #p"XFAIL"))
	      (pass-file (merge-pathnames-as-file policy-pathname #p"PASS"))
	      (fail-file (merge-pathnames-as-file policy-pathname #p"FAIL")))

	  (mapc (lambda (file)
		  (unless (file-exists-p (namestring file))
		    (error (format nil "Policy file \"~A\" missing." file))))
		(list xfail-file pass-file fail-file))
	  
	  (let ((p (make-instance 'policy)))
	    (setf (slot-value p 'commit-url-format) commit-url-format)
	    (setf (slot-value p 'xfail-matchers) (read-json-patterns :XFAIL xfail-file))
	    (setf (slot-value p 'pass-matchers) (read-json-patterns :PASS pass-file))
	    (setf (slot-value p 'fail-matchers) (read-json-patterns :FAIL fail-file))
	    p))))))

;; regex matcher for the special case of numeric ranges: two floating
;; point numbers separated by "..".
(defparameter *range-matcher*
  (cl-ppcre:create-scanner "^[0-9]+(|\.[0-9]*)\.\.[0-9]+(|\.[0-9]*)$"))

(defparameter *number-matcher*
  (cl-ppcre:create-scanner "^[0-9]+(|\.[0-9]*)"))

(defparameter *numeric-range*
  (cl-ppcre:create-scanner "^(.+)\.\.(.+)$"))

(defun parse-numeric-range (value)
  "Extract the numeric values for a double-dotted range."
  (multiple-value-bind (x y start-array end-array) 
      (cl-ppcre:scan *numeric-range* value)
    (values
     (read-from-string
      (subseq value (aref start-array 0) (aref end-array 0)))
     (read-from-string
      (subseq value (aref start-array 1) (aref end-array 1))))))

(defun compile-scanners (matcher)
  "Given an alist, MATCHER, replace that CDR of each pair with the
pre-compiled scanner of that regexp string.  The regexp string is
wrapped with ^ and $ to ensure an exact match.  As a special
exception, strings that match RANGE-MATCHER will be replaced by
RANGE-MATCHER."
  (mapcar (lambda (pair)
	    (cons (car pair)
		  (if (cl-ppcre:scan *range-matcher* (cdr pair))
		      ;; (multiple-value-bind (start end)
		      ;; 	  (parse-numeric-range (cdr pair))
		      ;; 	(XXXXXXXXX
		      (eval `(lambda (s)
			       (if (cl-ppcre:scan *number-matcher* s)
				   (let ((num (read-from-string s)))
				     ;; TODO verify num is within the range.
			       ))))
		      (eval `(lambda (s)
			       (cl-ppcre:scan
				,(cl-ppcre:create-scanner
				  (str:concat "^" (cdr pair) "$"))
				s))))))
	  matcher))

; (apply (cdr (car (cdr (compile-scanners '(( 1 . "a" ) (2 . "c")))))) '("c"))

(defun read-json-patterns (kind filename)
  (let ((patterns (list)))
    (let ((matcher-lines (inferior-shell:run/lines
			  (format nil "bash -c \"(cd $(dirname ~A); git blame -s -l $(basename ~A))\""
				  filename filename))))
      (mapc (lambda (matcher-line)
	      (let ((githash (subseq (remove #\^ matcher-line) 0 40)))
		(multiple-value-bind (lineno location)
		    (read-from-string (subseq matcher-line 40))
		  (let ((line (string-trim '(#\Space #\Tab)
					   (subseq matcher-line (+ 41 location)))))
		    (when (and (> (length line) 0)
			       (null (find (char line 0) "#;-")))
			(let ((json
			       (compile-scanners
				(json:decode-json-from-string line))))
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
		(when (and (null log-entry)
			   (not (string= githash ; check for local change
					 "0000000000000000000000000000000000000000")))
		  (progn
		    (setf log-entry (inferior-shell:run/lines
				     (format nil "bash -c \"(cd $(dirname ~A); git log -n 1 -r ~A $(basename ~A))\""
					     filename githash filename)))
		    (setf (gethash githash *git-log-table*) log-entry)))
		(setf (slot-value matcher 'log-entry) log-entry)))
	    patterns)

      patterns)))

(defun apply-policy (policy candidate-result-list)
  "Apply a POLICY to CANDIDATE-RESULT-LIST, a list of test results,
  where these results are represented as alists derived from json
  encoded values produced by the report parsers.  This function
  returns two values: :GREEN or :RED, as well as a list of pairs made
  by consing the matcher object with the test result alist."
  
  (let ((red-or-green :GREEN))
    (let ((result (mapcar (lambda (result)
			    (cons
			     (or
			      ;; Check for exceptions
			      (find-if (lambda (matcher)
					 (match-candidate-pattern
					  result (matcher matcher)))
				       (xfail-matchers policy))
			      ;; Now check for failures
			      (let ((red-match
				     (find-if (lambda (matcher)
						(match-candidate-pattern
						 result (matcher matcher)))
					      (fail-matchers policy))))
				(when red-match
				  (setf red-or-green :RED))
				red-match)
			      ;; No check for passes
			      (find-if (lambda (matcher)
					 (match-candidate-pattern
					  result (matcher matcher)))
				       (pass-matchers policy))
			      ;; We don't have a match. Let's fail.
			      (progn
				(setf red-or-green :RED)
				nil))
			     result))
			  candidate-result-list)))
      (values red-or-green result))))

