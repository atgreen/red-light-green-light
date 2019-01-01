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

;; Top level for rlgl-server

(in-package :rlgl-server)

(defvar *player-count* 5555)

;; ----------------------------------------------------------------------------
;; Storage backends

(defclass storage-backend ()
  ((key :initarg :key :reader key)))

(defvar *storage-driver*
  (make-instance 'local-storage-backend))
(init *storage-driver*)

;; ----------------------------------------------------------------------------
;; Parsing backends

(defclass report-parser ()
  ((name :initarg :name :reader name)))

;; ----------------------------------------------------------------------------
;; API routes

(snooze:defroute start (:get :text/plain)
  (setf *player-count* (+ 1 *player-count*))
  (format nil "~A" *player-count*))

(snooze:defroute evaluate (:post :application/json)
  (let ((json
	 (json:decode-json-from-string
	  (funcall
	   (read-from-string "hunchentoot:raw-post-data") :force-text t))))
    (let* ((doc (read-document *storage-driver* (cdr (assoc :REF json)))))
      (let ((pdoc (plump:parse doc))
	    (tests (list)))
	(lquery:$ pdoc "tr.resultbadA > td:nth-child(4) > a" 
		  (combine (attr :href) (text))
		  (map-apply #'(lambda (url text)
				 (setf tests
				       (cons
					(json:decode-json-from-string
					 (format nil "{ \"result\": \"FAIL\", \"id\": \"~A\", \"url\": \"~A\" }"
						 text url))
					tests)))))
	(if (null tests)
	    "ERROR"
	    (let ((processed-results (apply-policy *policy* tests)))
	      (format t "*****[~A]~%" processed-results)
	      (render processed-results)
	      (json:encode-json-to-string tests)))))))

(snooze:defroute upload (:post :application/octet-stream)
		 (store-document *storage-driver* (hunchentoot:raw-post-data)))

;;; END ROUTE DEFINITIONS -----------------------------------------------------

;;; Render processed results to HTML

(defmacro with-page ((&key title) &body body)
   `(with-html
      (:doctype)
      (:html
        (:head
         (:title ,title))
        (:body ,@body))))

(defun render (results)
  (with-open-file (stream "/tmp/rp.html"
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (let ((*html* stream))
      (with-page (:title "Report")
	(:header
	 (:style
"#results {
  font-family: \"Trebuchet MS\", Arial, Helvetica, sans-serif;
  border-collapse: collapse;
  width: 60%;
  margin-left: 20%;
  margin-right: 20%;
}

#results td, #results th {
  border: 1px solid #ddd;
  padding: 8px;
}

#results tr:nth-child(even){background-color: #f2f2f2;}

#results tr:hover {background-color: #ddd;}

#results th {
  padding-top: 12px;
  padding-bottom: 12px;
  text-align: left;
  background-color: #4CAF50;
  color: white;
}")
	 (:h1 "Report"))
	(:section
	 ("This is your report")
	 (:table :id "results"
		 (:body
		  (:tr (:th "RESULT") (:th "ID")) 
		  (dolist (item results)
		    (let ((matcher (car item))
			  (alist (cdr item)))
		      (:tr
		       (:td "FAIL")
		       (:td (:a :href (cdr (assoc :URL alist)) (cdr (assoc :ID alist))))))))))
	(:footer ("All done"))))))
	      
;;; Read JSON pattern ---------------------------------------------------------

;; Read policy files.  Ignore all blank lines and comments, which are
;; lines starting with #, ; or -.  Each json matcher should be on a
;; single line of text.  Record the line number of each matcher along
;; with the matcher.

(defvar *policy* nil)

; FIXME - above should be something like (make-policy #p".")

;;; HTTP SERVER CONTROL: ------------------------------------------------------
(defparameter *handler* nil)

(defmacro start-server (&key (handler '*handler*) (port 8080))
  "Initialize an HTTP handler"

  (setf snooze:*catch-errors* :verbose)
  (format t "PRE: ~A~%" hunchentoot:*dispatch-table*)
  (push (snooze:make-hunchentoot-app) hunchentoot:*dispatch-table*)
  (format t "POST: ~A~%" hunchentoot:*dispatch-table*)
  `(setf ,handler (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port ,port))))

(defmacro stop-server (&key (handler '*handler*))
  "Shutdown the HTTP handler"
  `(hunchentoot:stop ,handler))

;;; END SERVER CONTROL --------------------------------------------------------

(defun start-rlgl-server (arg)
  "Start the web application and have the main thread sleep forever,
  unless INTERACTIVE is nil."
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)
  (format t "WHHHHHHHHHHHHHHHHAAAAAAAAAAAATTTTTTTTTTTTT?")
  (start-server)
  (format t "OK?")
  ;; If ARG is NIL, then exit right away.  This is used by the
  ;; testsuite.
  (if arg
      (loop
	 (sleep 3000))))

(defun stop-rlgl-server ()
  "Stop the web application."
  (stop-server))
