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
		      (str:concat "rlgl-server:parser/" result))))))

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
    (let* ((doc (read-document *storage-driver* (cdr (assoc :REF json))))
	   (parser (recognize-report doc))
	   (tests (parse-report parser doc)))
      (if (null tests)
	  "ERROR"
	  (let ((processed-results (apply-policy *policy* tests)))
	    (let ((stream (make-string-output-stream)))
	      (render stream processed-results)
	      (format nil "green: ~A/doc?id=~A~%~%"
		      *server-uri*
		      (store-document *storage-driver*
				      (flexi-streams:string-to-octets
				       (get-output-stream-string stream))))))))))

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
	("@import url(\"https://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css\");

* {
  box-sizing: border-box;
}

body {
  padding: .2em 2em;
}

table {
  width: 100%;
}
table th {
  text-align: left;
  border-bottom: 1px solid #ccc;
}
table th, table td {
  padding: .4em;
}

table.fold-table > tbody > tr.view td, table.fold-table > tbody > tr.view th {
  cursor: pointer;
}
table.fold-table > tbody > tr.view td:first-child,
table.fold-table > tbody > tr.view th:first-child {
  position: relative;
  padding-left: 20px;
}
table.fold-table > tbody > tr.view td:first-child:before,
table.fold-table > tbody > tr.view th:first-child:before {
  position: absolute;
  top: 50%;
  left: 5px;
  width: 9px;
  height: 16px;
  margin-top: -8px;
  font: 16px fontawesome;
  color: #999;
  content: \"\\f0d7\";
  transition: all .3s ease;
}
table.fold-table > tbody > tr.view:nth-child(4n-1) {
  background: #eee;
}
table.fold-table > tbody > tr.view:hover {
  background: #000;
}
table.fold-table > tbody > tr.view.open {
  background: tomato;
  color: white;
}
table.fold-table > tbody > tr.view.open td:first-child:before, table.fold-table > tbody > tr.view.open th:first-child:before {
  transform: rotate(-180deg);
  color: #333;
}
table.fold-table > tbody > tr.fold {
  display: none;
}
table.fold-table > tbody > tr.fold.open {
  display: table-row;
}

.fold-content {
  padding: .5em;
}
.fold-content h3 {
  margin-top: 0;
}
.fold-content > table {
  border: 2px solid #ccc;
}
.fold-content > table > tbody tr:nth-child(even) {
  background: #eee;
}")
(:script :src "https://cdnjs.cloudflare.com/ajax/libs/prefixfree/1.0.7/prefixfree.min.js"))


       (:h1 "Report"))
      (:section
       ("This is your report")
       (:table :class "fold-table" :id "results"
	       (:body
		(:tr (:th "RESULT") (:th "ID"))
		(dolist (item results)
		  (let ((matcher (car item))
			(alist (cdr item)))
		    (:tr :class "view"
		     (:td (cdr (assoc :RESULT alist)))
		     (:td (:a :href (cdr (assoc :URL alist)) (cdr (assoc :ID alist)))))
		    (:tr :class "fold"
			 (:td :colspan "2")
			 (:div :class "fold-content"
			       (:h3 "more info")))
		    ))))
       (:script :attrs (list :src "http://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js"))
       (:script :attrs (list :src "js/index.js"))
      (:footer ("All done"))))))
	      
;;; Read JSON pattern ---------------------------------------------------------

;; Read policy files.  Ignore all blank lines and comments, which are
;; lines starting with #, ; or -.  Each json matcher should be on a
;; single line of text.  Record the line number of each matcher along
;; with the matcher.

(defvar *policy* nil)

;;; HTTP SERVER CONTROL: ------------------------------------------------------
(defparameter *handler* nil)

(defun rlgl-root ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))

(defparameter *rlgl-dispatch-table*
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory
                (make-pathname :name "images"
                               :defaults (rlgl-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (fad:pathname-as-directory
            (make-pathname :name "js"
                           :defaults (rlgl-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory
            (make-pathname :name "css"
                           :defaults (rlgl-root))))
   (snooze:make-hunchentoot-app)))

(defmacro start-server (&key (handler '*handler*) (port 8080))
  "Initialize an HTTP handler"
  `(progn
     (setf snooze:*catch-errors* :verbose)
     (setf hunchentoot:*dispatch-table* *rlgl-dispatch-table*)
     (setf ,handler (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
						      :document-root #p"./"
						      :port ,port)))))

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
  (log:info *default-config-text*)

  ;; FIXME: lookup storage driver
  ;; (setf *storage-driver (fixme-lookup (gethash "storage-driver" *config*)))
  (setf *server-uri* (gethash "server-uri" *config*))
  (log:info *server-uri*)
  
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
