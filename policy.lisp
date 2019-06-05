;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018, 2019  Anthony Green <green@moxielogic.com>
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

;; Matcher routines

(defpackage #:policy
  (:use #:cl #:matcher #:cl-fad)
  (:shadow #:package)
  (:export #:*policy-dir* #:make-policy #:apply-policy
	   #:commit-url-format #:version #:compile-scanners))

(in-package #:policy)

(defvar *policy-lock* (bt:make-lock))

(defvar *policy-dir* nil)

(defvar *git-log-table* (make-hash-table :test 'equal))
(defvar *git-commit-table* (make-hash-table :test 'equal))

(defclass policy ()
  ((version :reader version)
   (xfail-matchers :reader xfail-matchers)
   (pass-matchers  :reader pass-matchers)
   (fail-matchers  :reader fail-matchers)
   (commit-url-format :reader commit-url-format)))

(defun guess-commit-url-format (url)
  "Guess the commit URL format string based on URL."
  ;; They mostly look like this (gogs & github).  Haven't seen
  ;; anything different yet...
  (str:concat  (if (str:ends-with? ".git" url)
		   (str:substring 0 (- (length url) 5) url)
		   url)
	       "/commit/~A"))

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

      (unless (fad:directory-exists-p policy-dirname)
	(let ((output (if (not (fad:directory-exists-p policy-dirname))
			  (inferior-shell:run
			   (format nil "/usr/bin/git clone ~A ~A"
				   url policy-dirname))
			  (inferior-shell:run
      			   (format nil "bash -c \"(cd ~A; git pull)\""
      				   policy-dirname)))))
	  (mapc (lambda (line)
		  (print line))
		output)))

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
	    (setf (slot-value p 'version)
		  (inferior-shell:run/ss
		   (format nil "bash -c \"(cd ~A; git rev-parse HEAD)\""
			   policy-dirname)))
	    (setf (slot-value p 'commit-url-format) commit-url-format)
	    (setf (slot-value p 'xfail-matchers) (read-json-patterns :XFAIL xfail-file))
	    (setf (slot-value p 'pass-matchers) (read-json-patterns :PASS pass-file))
	    (setf (slot-value p 'fail-matchers) (read-json-patterns :FAIL fail-file))

	    ;; Map all of the commit hashes to this policy for future
	    ;; reference.
	    (let ((hash-log (inferior-shell:run/lines
			     (format nil "bash -c \"(cd ~A; git log --pretty=%H)\""
				     policy-dirname))))
	      (mapc (lambda (key)
		      (setf (gethash key *git-commit-table*) p))
		    hash-log))

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
  "Given an alist, MATCHER, replace the CDR of each pair with a
function that runs one of three matching algorithms against a single
string argument: numeric range checks, regexp matching, and string
comparison.  The algorithm choice depends on the CDR of the pair,
which is either a numeric range string ('..' notation), a
regexp (starting with \#^), or any other string."

  (mapcar (lambda (pair)
	    (cons (car pair)
		  (cond
		    ((cl-ppcre:scan *range-matcher* (cdr pair))
		     (multiple-value-bind (start end)
		       	 (parse-numeric-range (cdr pair))
		       (eval `(lambda (s)
				(and (cl-ppcre:scan *number-matcher* s)
				     (let ((num (read-from-string s)))
				       (and (>= num ,start)
					    (<= num ,end))))))))
		    ((eq (char (cdr pair) 0) #\^)
		      (eval `(lambda (s)
			       (cl-ppcre:scan
				,(cl-ppcre:create-scanner
				  (str:concat (cdr pair) "$"))
				s))))
		    (t
		      (eval `(lambda (s)
			       (string= s ,(cdr pair))))))))
	  matcher))

(defun extract-expiration-date (line)
  "Given a matcher string LINE, return a universal time value if one
exists after the JSON object, or NIL otherwise."
  (let* ((last-brace (position #\} line :from-end t))
	 (date-string (str:trim (str:substring (+ last-brace 1) t line))))
    (and (> (length date-string) 0)
	 (date-time-parser:parse-date-time date-string))))

;; An arbitratily far-away date, representing "never"
(defparameter *the-year-3000* (date-time-parser:parse-date-time "3000"))

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
								    :matcher json
								    :expiration-date (or (extract-expiration-date line)
											 *the-year-3000*))
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
  
  (let ((red-or-green :GREEN)
	(now (get-universal-time)))
    (let ((result (mapcar (lambda (result)
			    (cons
			     (or
			      ;; Check for exceptions
			      (find-if (lambda (matcher)
					 (and (match-candidate-pattern
					       result (matcher matcher))
					      (< now (expiration-date matcher))))
				       (xfail-matchers policy))
			      ;; Now check for failures
			      (let ((red-match
				     (find-if (lambda (matcher)
						(and (match-candidate-pattern
						      result (matcher matcher))
						     (< now (expiration-date matcher))))
					      (fail-matchers policy))))
				(when red-match
				  (setf red-or-green :RED))
				red-match)
			      ;; No check for passes
			      (find-if (lambda (matcher)
					 (and (match-candidate-pattern
					       result (matcher matcher))
					      (< now (expiration-date matcher))))
				       (pass-matchers policy))
			      ;; We don't have a match. Let's fail.
			      (progn
				(setf red-or-green :RED)
				nil))
			     result))
			  candidate-result-list)))
      (values red-or-green result))))



