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

;; Top level for rlgl-server

(in-package :rlgl-server)

;; ----------------------------------------------------------------------------
;; Default configuration.  Overridden by external config file.
(defvar *config* nil)
(defparameter *default-config-text*
"storage-driver = \"local\"
server-uri = \"http://localhost:8080\"
")

(defvar *server-uri* nil)

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

;; Run all of the scripts in recog.d until we find
;; a match.
(defun recognize-report (doc)
  (let ((fname
	 (cl-fad:with-output-to-temporary-file (stream)
	   (print doc stream))))
    (let ((scripts (cl-fad:list-directory "recog.d"))
	  (result nil))
      (find-if (lambda (script)
		 (let ((output (inferior-shell:run/ss
				(str:concat
				 (namestring script) " "
				 (namestring fname)))))
		   (setf result output)
		   (equal (length output) 0)))
	       scripts)
      (delete-file fname)
      (make-instance (read-from-string
		      (str:concat "parser/" result))))))

;; ----------------------------------------------------------------------------
;; API routes

(snooze:defroute start (:get :text/plain)
  ; Return a random 7 character hash
  (let ((chars "abcdef0123456789"))
    (coerce (loop repeat 7 collect (aref chars (random (length chars))))
            'string)))

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
	      (let ((stream (make-string-output-stream)))
		(render stream processed-results)
		(format nil "green: ~A/doc?id=~A~%~%"
			*server-uri*
			(store-document *storage-driver*
					(flexi-streams:string-to-octets
					 (get-output-stream-string stream)))))))))))

(snooze:defroute upload (:post :application/octet-stream)
  (store-document *storage-driver* (hunchentoot:raw-post-data)))

(snooze:defroute doc (:get :text/html &key id)
  (read-document *storage-driver* id))

;;; END ROUTE DEFINITIONS -----------------------------------------------------

;;; Render processed results to HTML

(defmacro with-page ((&key title) &body body)
   `(with-html
      (:doctype)
      (:html
        (:head
         (:title ,title))
        (:body ,@body))))

(defun render (stream results)
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
      (:footer ("All done")))))
	      
;;; Read JSON pattern ---------------------------------------------------------

;; Read policy files.  Ignore all blank lines and comments, which are
;; lines starting with #, ; or -.  Each json matcher should be on a
;; single line of text.  Record the line number of each matcher along
;; with the matcher.

(defvar *policy* nil)

;;; HTTP SERVER CONTROL: ------------------------------------------------------
(defparameter *handler* nil)

(defmacro start-server (&key (handler '*handler*) (port 8080))
  "Initialize an HTTP handler"
  `(progn
     (setf snooze:*catch-errors* :verbose)
     (push (snooze:make-hunchentoot-app) hunchentoot:*dispatch-table*)
     (setf ,handler (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port ,port)))))

(defmacro stop-server (&key (handler '*handler*))
  "Shutdown the HTTP handler"
  `(hunchentoot:stop ,handler))

;;; END SERVER CONTROL --------------------------------------------------------

(defun start-rlgl-server (arg)
  "Start the web application and have the main thread sleep forever,
  unless INTERACTIVE is nil."
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)

  ;; Read the built-in configuration settings.
  (setf *config* (cl-toml:parse *default-config-text*))

  ;; FIXME: lookup storage driver
  ;; (setf *storage-driver (fixme-lookup (gethash "storage-driver" *config*)))
  (setf *server-uri* (gethash "server-uri" *config*))
  
  (setf *policy* (make-policy
		  "https://gogs-labdroid.apps.home.labdroid.net/green/test-policy.git"))
  (let ((srvr (start-server)))
    ;; If ARG is NIL, then exit right away.  This is used by the
    ;; testsuite.
    (if arg
	(loop
	   (sleep 3000)))
    srvr))

(defun stop-rlgl-server ()
  "Stop the web application."
  (stop-server))
