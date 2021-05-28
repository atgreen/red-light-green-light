;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2018-2021  Anthony Green <green@moxielogic.com>
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

;; Top level for rlgl-server

(in-package :rlgl-server)

;; ----------------------------------------------------------------------------
;; Get the version number at compile time.  This comes from
;; RLGL_VERSION (set on the linux container build commandline), or
;; from git at compile-time.  Use UNKNOWN if all else fails.

;; This can come from build time...
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter +rlgl-git-version+
    (inferior-shell:run/ss
     "(test -d .git && git describe --tags --dirty=+) || echo UNKNOWN")))

;; But this must come from runtime...
(defparameter +rlgl-version+
  (let ((v +rlgl-git-version+))
    (if (equal v "UNKNOWN")
 	(or (uiop:getenv "RLGL_VERSION") v)
 	v)))

;; ----------------------------------------------------------------------------
;; Default configuration.  Overridden by external config file.

(defvar *config* nil)
(defvar *default-config* nil)
(defparameter +default-config-text+
"storage-driver = \"local\"
server-uri = \"http://localhost:8080\"
policy-dir = \"/var/rlgl/policy/\"
db = \"sqlite\"
sqlite-db-filename = \"/var/rlgl/rlgl.db\"
postgresql-host = \"localhost\"
postgresql-port = 5432
private-key-file = \"/etc/rlgl-signer/rlgl-signer-private-key.pem\"
public-key-file = \"/etc/rlgl-signer/rlgl-signer-public-key.pem\"
keycloak-oidc-realm-uri = \"ignore\"
keycloak-oidc-realm-redirect-uri = \"ignore\"
keycloak-oidc-client-id = \"ignore\"
keycloak-oidc-client-secret = \"ignore\"
")

(defvar *server-uri* nil)
(defvar *keycloak-oidc-realm-redirect-uri* nil)
(defvar *keycloak-oidc-realm-uri* nil)
(defvar *keycloak-oidc-client-id* nil)
(defvar *keycloak-oidc-client-secret* nil)

;; ----------------------------------------------------------------------------
(defparameter *rlgl-registry* nil)
(defparameter *http-requests-counter* nil)
(defparameter *http-request-duration* nil)

(defun initialize-metrics ()
  (unless *rlgl-registry*
    (setf *rlgl-registry* (prom:make-registry))
    (let ((prom:*default-registry* *rlgl-registry*))
      (setf *http-requests-counter*
            (prom:make-counter :name "http_requests_total"
                               :help "Counts http request by type"
                               :labels '("method" "app")))
      (setf *http-request-duration*
	    (prom:make-histogram :name "http_request_duration_milliseconds"
                                 :help "HTTP requests duration[ms]"
                                 :labels '("method" "app")
                                 :buckets '(10 25 50 75 100 250 500 750 1000 1500 2000 3000)))
      #+sbcl
      (prom.sbcl:make-memory-collector)
      #+sbcl
      (prom.sbcl:make-threads-collector)
      (prom.process:make-process-collector))))

;; ----------------------------------------------------------------------------
;; Document storage backends
;;

(defclass storage-backend ()
  ((key :initarg :key :reader key)))

(defvar *storage-driver* nil)

;; ----------------------------------------------------------------------------
;; Database backends

(defvar *db* nil)

;; ----------------------------------------------------------------------------
;; Parsing backends

(defclass report-parser ()
  ((name :initarg :name :reader name)
   (title :initarg :title :reader title)
   (doctype :initarg :doctype :reader doctype)))

;; Run all of the scripts in recog.d until we find
;; a match.
(defun recognize-report (doc)
  "Try to recognize the report type in the string DOC.  If we
recognize it, return a RLGL-SERVER:PARSER object, NIL otherwise."
  (let ((fname
	  (cl-fad:with-output-to-temporary-file (stream
						 :element-type '(unsigned-byte 8))
	    (write-sequence doc stream)))
	(result nil))
    (unwind-protect
	 (progn
	   (let ((scripts (cl-fad:list-directory
			   (fad:pathname-as-directory
			    (make-pathname :name "recog"
					   :type "d"
					   :defaults (rlgl-root))))))
	     ;; This could probably be a loop over scripts, where we
	     ;; return output instead of setting result.  The
	     ;; unwind-protect would make sure that the file was
	     ;; deleted.
	     (find-if (lambda (script)
			(log:info "Testing '~A'" script)
			(let ((output (inferior-shell:run/ss
				       (str:concat
					(namestring script) " "
					(namestring fname)))))
			  (setf result output)
			  (> (length output) 0)))
		      scripts)))
      (when fname
	(delete-file fname)))
    (when (> (length result) 0)
      (make-instance (read-from-string
		      (str:concat "rlgl-server:parser/" result))))))

;; ----------------------------------------------------------------------------
;; HTML rendering helpers...

(defun emit-bootstrap.min.css ()
  (list :rel "stylesheet"
	:href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
	:integrity "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
	:crossorigin "anonymous"))

(defun emit-bootstrap.min.js ()
  (list :src "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
	:integrity "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
	:crossorigin "anonymous"))

;; ----------------------------------------------------------------------------
;; Track actions via matomo

(defvar *matomo-uri* nil)
(defvar *matomo-idsite* nil)
(defvar *matomo-token-auth* nil)

(defvar *thread-pool* (thread-pool:make-thread-pool 10))

(defun track-action (action &key url api-key)
  (when *matomo-uri*
    (let* ((request hunchentoot:*request*)
           (parameters `(("idsite" . ,*matomo-idsite*)
                         ("token_auth" . ,*matomo-token-auth*)
                         ("rand" . ,(rlgl.util:random-hex-string))
                         ("ua" . ,(hunchentoot:user-agent request))
                         ("action_name" . ,action)
                         ("ref" . ,(hunchentoot:header-in :HTTP_REFERER request))
                         ("cip" . ,(hunchentoot:real-remote-addr request))
                         ("rec" . "1")
                         ("apiv" . "1"))))
      (when api-key
        (setf parameters (cons (cons "dimension2" api-key)
                               parameters)))
      (when url
        (setf parameters (cons (cons "url" (str:concat *server-uri* url))
                               parameters)))
      (thread-pool:add-to-pool
       *thread-pool*
       (lambda ()
         (drakma:http-request *matomo-uri*
                              :method :post
                              :parameters parameters))))))


(defvar *private-key-file* "/etc/rlgl-signer/rlgl-signer-private-key.pem")
(defvar *public-key-file* "/etc/rlgl-signer/rlgl-signer-public-key.pem")
(defvar *public-key* (car (inferior-shell:run/lines
                            (format nil "sh -c 'cat ~A | base64 -w0'" *public-key-file*))))

(defun make-string-signature (s)
  "Generate a detached signature for S."
  (let* ((cmd (format nil "sh -c 'openssl dgst -sha256 -sign ~A - | base64 -w0'" *private-key-file*))
         (signature (with-input-from-string (stream s)
                      (car (inferior-shell:run cmd
                                               :output :lines :input stream)))))
    (if (null signature)
        (error "Internal error generating document signature for ~A" s)
        signature)))

(defvar *rekor-uri* "https://rekor.sigstore.dev/api/v1/log/entries")

(defun rekor-envelope (envelope signature)
  (when *rekor-uri*
    (let ((data (json:encode-json-to-string
                  `((:API-VERSION . "0.0.1")
                    (:KIND . "rekord")
                    (:SPEC (:SIGNATURE (:FORMAT . "x509")
                                       (:CONTENT . ,signature)
                                       (:PUBLIC-KEY (:CONTENT . ,*public-key*)))
                           (:DATA (:CONTENT . ,(cl-base64:string-to-base64-string envelope))))))))
      (multiple-value-bind (a b c d e f g)
        (drakma:http-request *rekor-uri*
                             :accept "application/json"
                             :method :post
                             :content-type "application/json"
                             :external-format-out :utf-8
                             :external-format-in :utf-8
                             :redirect 100
                     :content data)
       (let ((result (flexi-streams:octets-to-string a :external-format :utf-8)))
          (log:info result))))))

;; ----------------------------------------------------------------------------
;; API authentication

(defun authorize ()
  (let* ((request hunchentoot:*request*)
	 (access-token (hunchentoot:header-in :AUTHORIZATION request))
	 (token-type-and-value (split-sequence:split-sequence #\space access-token))
	 (token-type (first token-type-and-value))
	 (token-string (second token-type-and-value)))
    ;; Make sure that it is a bearer token
    (if (and (equalp token-type "Bearer")
             (rlgl.api-key:authorize-by-api-key *db* token-string))
        (track-action "authorize" :api-key token-string)
      (error "Authorization error"))))

(defun authorize-policy-bound-api-key (policy-name)
  (let* ((request hunchentoot:*request*)
	 (access-token (hunchentoot:header-in :AUTHORIZATION request))
	 (token-type-and-value (split-sequence:split-sequence #\space access-token))
	 (token-type (first token-type-and-value))
	 (token-string (second token-type-and-value)))
    ;; Make sure that it is a bearer token
    (unless (and (equalp token-type "Bearer")
		 (rlgl.api-key:authorize-by-policy-bound-api-key *db* token-string policy-name))
      (error "Authorization error"))))

;; ----------------------------------------------------------------------------
;; API routes

(setf snooze:*home-resource* :index)

;; Readiness probe
(snooze:defroute healthz (:get :text/*)
  "ready")

(snooze:defroute home (:get :text/*)
  (log:info "ROOT")
  (track-action "home" :url "/")
  (with-html-string
    (:doctype)
    (:html
       (:head
	(:meta :charset "utf-8")
	(:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
	(:link :rel "apple-touch-icon" :sizes "180x180" :href "images/apple-touch-icon.png")
	(:link :rel "icon" :type "image/png" :sizes "32x32" :href "images/favicon-32x32.png")
	(:link :rel "icon" :type "image/png" :sizes "16x16" :href "images/favicon-16x16.png")
	(:link :rel "manifest" :href "images/site.webmanifest")
	(:link :rel "mask-icon" :href "images/safari-pinned-tab.svg")
	(:meta :name "msapplication-TileColor" :content "#da532c")
	(:meta :name "theme-color" :content "#ffffff")
        (:title "Red Light Green Light")
	(:link :rel "stylesheet" :href "css/rlgl.css")
	(:link :attrs (emit-bootstrap.min.css))
	(:script :src "https://cdnjs.cloudflare.com/ajax/libs/prefixfree/1.0.7/prefixfree.min.js"))
       (:body
	(:header
	 (:nav :class "navbar navbar-expand-md navbar-dark fixed-top bg-dark"
	       (:a :class "navbar-brand"
		   :href *server-uri* "Red Light Green Light")))
	(:main :role "main" :class "container"
	       (:div :class "row"
		     (:div :class "col"
			   (:div :style "width:100px"
				 (:div :class "rlgl-svg"))
			   (:h1 :class "mt-5" "Welcome!")
			   (:br)
			   "Red Light Green Light is a git-centric tool designed to accelerate CI/CD pipelines."
			   (:br)
			   (:br)
			   "This hosted version of Red Light Green Light is an experimental service and offers no guarantees.  Use at your own risk.  Note that all documents and reports will age-out after 30 days."
			   (:br)
			   (:br)
			   (:h4 "Downloads")
			   (:ul
			    (:li (:a :href "cli/rlgl-linux-amd64.tgz" "rlgl command-line tool for 64-bit x86 Linux"))
			    (:li (:a :href "cli/rlgl-linux-arm.tgz" "rlgl command-line tool for 64-bit ARM Linux"))
			    (:li (:a :href "cli/rlgl-linux-ppc64le.tgz" "rlgl command-line tool for 64-bit Little-Endian Power Linux"))
			    (:li (:a :href "cli/rlgl-linux-s390x.tgz" "rlgl command-line tool for s390x Linux"))
			    (:li (:a :href "cli/rlgl-darwin-amd64.tgz" "rlgl command-line tool for 64-bit x86 OSX"))
			    (:li (:a :href "cli/rlgl-windows-amd64.zip" "rlgl command-line tool for 64-bit x86 Windows")))
			   (:br)
			   (:h4 "Public Signing Key")
                           "The public signing key used for " (:a :href "https://sigstore.dev/" "sigstore") " archiving is available by " (:a :href "/pubkey" "clicking here") "."
			   (:br)
			   (:br)
			   (:h4 "Documentation")
			   "Documentation is found in the " (:a :href "https://github.com/atgreen/red-light-green-light/blob/master/README.md" "Red Light Green Light source README file") "."
			   (:br)
			   (:br)
			   (:h4 "Reporting Issues")
			   "Please feel free to ask questions and report issues here: "(:a :href "https://github.com/atgreen/red-light-green-light/issues" "https://github.com/atgreen/red-light-green-light/issues") "."
			   (:br)
			   (:br)
			   (:hr)
			   "Red Light Green Light was written by Anthony Green "
			   (:a :href "mailto:green@moxielogic.com" "<green@moxielogic.com>")
			   " and is available in source form under the terms of the AGPLv3 license from "
			   (:a :href "https://github.com/atgreen/red-light-green-light" "https://github.com/atgreen/red-light-green-light") "."
			   )))
	(:footer :class "page-footer font-small special-color-dark pt-4"
		 (:div :class "footer-copyright text-center py-3" "Version" +rlgl-version+ "   //   (C) 2018-2021"
		       (:a :href "https://linkedin.com/in/green" " Anthony Green"))))
       (:script :attrs (list :src "https://code.jquery.com/jquery-3.3.1.slim.min.js"
			     :integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
			     :crossorigin "anonymous"))
       (:script :attrs (list :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js"
			     :integrity "sha384-wHAiFfRlMFy6i5SRaxvfOCifBUQy1xHdJ/yoi7FRNXMRBu5WHdZYu1hA6ZOblgut"
			     :crossorigin "anonymous"))
       (:script :attrs (emit-bootstrap.min.js)))))

;; Render the home page.
(snooze:defroute index (:get :text/*)
  (log:info "ROOT2")
  (track-action "home" :url "/")
  (with-html-string
    (:doctype)
    (:html
       (:head
	(:meta :charset "utf-8")
	(:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
	(:link :rel "apple-touch-icon" :sizes "180x180" :href "images/apple-touch-icon.png")
	(:link :rel "icon" :type "image/png" :sizes "32x32" :href "images/favicon-32x32.png")
	(:link :rel "icon" :type "image/png" :sizes "16x16" :href "images/favicon-16x16.png")
	(:link :rel "manifest" :href "images/site.webmanifest")
	(:link :rel "mask-icon" :href "images/safari-pinned-tab.svg")
	(:meta :name "msapplication-TileColor" :content "#da532c")
	(:meta :name "theme-color" :content "#ffffff")
        (:title "Red Light Green Light")
	(:link :rel "stylesheet" :href "css/rlgl.css")
	(:link :attrs (emit-bootstrap.min.css))
	(:script :src "https://cdnjs.cloudflare.com/ajax/libs/prefixfree/1.0.7/prefixfree.min.js"))
       (:body
	(:header
	 (:nav :class "navbar navbar-expand-md navbar-dark fixed-top bg-dark"
	       (:a :class "navbar-brand"
		   :href *server-uri* "Red Light Green Light")))
	(:main :role "main" :class "container"
	       (:div :class "row"
		     (:div :class "col"
			   (:div :style "width:100px"
				 (:div :class "rlgl-svg"))
			   (:h1 :class "mt-5" "Welcome!")
			   (:br)
			   "Red Light Green Light is a git-centric tool designed to accelerate CI/CD pipelines."
			   (:br)
			   (:br)
			   "This hosted version of Red Light Green Light is an experimental service and offers no guarantees.  Use at your own risk.  Note that all documents and reports will age-out after 30 days."
			   (:br)
			   (:br)
			   (:h4 "Downloads")
			   (:ul
			    (:li (:a :href "cli/rlgl-linux-amd64.tgz" "rlgl command-line tool for 64-bit x86 Linux"))
			    (:li (:a :href "cli/rlgl-linux-arm.tgz" "rlgl command-line tool for 64-bit ARM Linux"))
			    (:li (:a :href "cli/rlgl-linux-ppc64le.tgz" "rlgl command-line tool for 64-bit Little-Endian Power Linux"))
			    (:li (:a :href "cli/rlgl-linux-s390x.tgz" "rlgl command-line tool for s390x Linux"))
			    (:li (:a :href "cli/rlgl-darwin-amd64.tgz" "rlgl command-line tool for 64-bit x86 OSX"))
			    (:li (:a :href "cli/rlgl-windows-amd64.zip" "rlgl command-line tool for 64-bit x86 Windows")))
			   (:br)
			   (:h4 "Public Signing Key")
                           "The public signing key used for " (:a :href "https://sigstore.dev/" "sigstore") " archiving is available by " (:a :href "/pubkey" "clicking here") "."
			   (:br)
			   (:br)
			   (:h4 "Documentation")
			   "Documentation is found in the " (:a :href "https://github.com/atgreen/red-light-green-light/blob/master/README.md" "Red Light Green Light source README file") "."
			   (:br)
			   (:br)
			   (:h4 "Reporting Issues")
			   "Please feel free to ask questions and report issues here: "(:a :href "https://github.com/atgreen/red-light-green-light/issues" "https://github.com/atgreen/red-light-green-light/issues") "."
			   (:br)
			   (:br)
			   (:hr)
			   "Red Light Green Light was written by Anthony Green "
			   (:a :href "mailto:green@moxielogic.com" "<green@moxielogic.com>")
			   " and is available in source form under the terms of the AGPLv3 license from "
			   (:a :href "https://github.com/atgreen/red-light-green-light" "https://github.com/atgreen/red-light-green-light") "."
			   )))
	(:footer :class "page-footer font-small special-color-dark pt-4"
		 (:div :class "footer-copyright text-center py-3" "Version" +rlgl-version+ "   //   (C) 2018-2021"
		       (:a :href "https://linkedin.com/in/green" " Anthony Green"))))
       (:script :attrs (list :src "https://code.jquery.com/jquery-3.3.1.slim.min.js"
			     :integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
			     :crossorigin "anonymous"))
       (:script :attrs (list :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js"
			     :integrity "sha384-wHAiFfRlMFy6i5SRaxvfOCifBUQy1xHdJ/yoi7FRNXMRBu5WHdZYu1hA6ZOblgut"
			     :crossorigin "anonymous"))
       (:script :attrs (emit-bootstrap.min.js)))))

(snooze:defroute start (:get :text/plain)
  (authorize)
  ;; Return a random 7 character hash
  (rlgl.util:random-hex-string 7))

(snooze:defroute login (:get :text/plain)
  (track-action "login" :url "/login")
  (format nil "rlgl-server connected -- version ~A" +rlgl-version+))

(snooze:defroute report-log (:get :text/plain &key id)
  ;;  (authorize)
  (track-action "log")
  (rlgl.db:report-log *db* id))

(defun base64-decode (base-64-string)
  "Takes a base64-uri string and return an array of octets"
  (flet ((round-up-by-4 (n)
           (let ((r (rem n 4)))
             (if (> r 0)
                 (- 4 r)
                 0))))
     ;; Re-pad the string, or CL-BASE64 will get confused
    (let ((s (concatenate 'string
                          base-64-string
                          (make-array (round-up-by-4 (length base-64-string))
                                      :element-type 'character
                                      :initial-element #\.))))
      (cl-base64:base64-string-to-usb8-array s :uri t))))

(defun decode-jwt (jwt-string)
  "Decodes a JSON Web Token. Returns two alists,
token claims and token header"
  (destructuring-bind (header-string claims-string digest-string)
      (split-sequence:split-sequence #\. jwt-string)
    (let* ((headers (json:decode-json-from-string
		     (flexi-streams:octets-to-string
		      (base64-decode
		       header-string)
		      :external-format :utf-8)))
           (claims (json:decode-json-from-string
		    (flexi-streams:octets-to-string
		     (base64-decode
		      claims-string)
		     :external-format :utf-8))))
      (values headers claims))))

(snooze:defroute get-baseline-xfail-policy (:get :text &key id)
  (authorize)
  (let ((id (string id)))
    ;; Accept full URLs, in which case we extract the document id from the end.
    (when (str:starts-with? *server-uri* id)
      (setf id (str:substring (- (length id) 13) nil id)))
    (handler-case
	(let ((doc
		(flexi-streams:octets-to-string
		 (read-document *storage-driver* id)
		 :external-format :utf-8))
	      (stream (make-string-output-stream)))
	  (loop with index = 0
		for pos = (search "<td>FAIL" doc :start2 index)
		when pos do (setf index (+ 1 pos))
		when pos do (let ((start (+ 1 (search "{" doc :start2 pos)))
				  (end (search "}</pre>" doc :start2 pos)))
			      (format stream "{ ~A }~%"
				      (string-trim " "
						   (cl-ppcre:regex-replace-all
						    "    "
						    (substitute #\SPACE #\NEWLINE (subseq doc start end)) ""))))
		while pos)
	  (get-output-stream-string stream))
      (error (c)
	(log:error "~A" c)
	(format nil "# Could not generate baseline regression policy for ~A~%" id)))))

(snooze:defroute get-api-key (:get :text/html &key code session_state)
  (if code
      (progn
        (track-action "get-api-key" :url "/get-api-key")
	(log:info "Got a code: ~A" (string-downcase code))
	(let* ((token (flexi-streams:octets-to-string
		       (drakma:http-request (str:concat *keycloak-oidc-realm-uri* "/protocol/openid-connect/token")
					    :method :post
					    :parameters `(("client_id" . ,*keycloak-oidc-client-id*)
							  ("client_secret" . ,*keycloak-oidc-client-secret*)
							  ("grant_type" . "authorization_code")
							  ("redirect_uri" . ,(format nil "~A/get-api-key" *server-uri*))
							  ("scope" . "openid")
							  ("code" . ,(string-downcase code))))
		       :external-format :utf-8))
	       (json (json:decode-json-from-string token)))
	  ;; FIXME - deal with bad logins
	  (multiple-value-bind (headers claims)
	      (decode-jwt (cdr (assoc :ID--TOKEN json)))
	    (let ((user (rlgl.user:find-user-by-keycloak-id-token *db* claims)))
	      (with-html-string
		(:doctype)
		(:html
		 (:head
		  (:meta :charset "utf-8")
		  (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
		  (:link :rel "icon" :href "images/rlgl.svg.png")
		  (:title "Red Light Green Light")
		  (:link :rel "stylesheet" :href "css/rlgl.css")
		  (:link :attrs (emit-bootstrap.min.css))
		  (:script :src "https://cdnjs.cloudflare.com/ajax/libs/prefixfree/1.0.7/prefixfree.min.js"))
		 (:body
		  (:header
		   (:nav :class "navbar navbar-expand-md navbar-dark fixed-top bg-dark"
			 (:a :class "navbar-brand"
			     :href "https://github.com/atgreen/red-light-green-light" "Red Light Green Light")))
		  (:main :role "main" :class "container"
			 (:div :class "row"
			       (:div :class "col"
				     (:div :class "alert alert-warning alert-dismissible fade show" :role "alert"
					   "You are logged in as user " (rlgl.user:user-name user) "."
					   (:button :type "button"
						    :class "close"
						    :data-dismiss "alert"
						    :aria-label "Close"
						    (:span :aria-hidden "true"
							   "X")))
				     (:div :style "width:100px"
					   (:div :class "rlgl-svg"))
				     (:h1 :class "mt-5" "Your personal API key")
				     (:br)
				     "Your personal API key is "
				     (:b (rlgl.user:user-api-key user) ".")
				     (:br)
				     (:br)
				     "Use the following command to login to this server:"
				     (:pre
				      (format nil "rlgl login --key ~A ~A"
					      (rlgl.user:user-api-key user)
					      *server-uri*))
				     (:br)
				     (:hr)
				     "Red Light Green Light was written by Anthony Green "
				     (:a :href "mailto:green@moxielogic.com" "<green@moxielogic.com>")
				     " and is available in source form under the terms of the AGPLv3 license from "
				     (:a :href "https://github.com/atgreen/red-light-green-light" "https://github.com/atgreen/red-light-green-light") "."
				     )))
		  (:footer :class "page-footer font-small special-color-dark pt-4"
			   (:div :class "footer-copyright text-center py-3" "Version" +rlgl-version+ "   //   (C) 2018-2021"
				 (:a :href "https://linkedin.com/in/green" " Anthony Green"))))
		 (:script :attrs (list :src "https://code.jquery.com/jquery-3.3.1.slim.min.js"
				       :integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
				       :crossorigin "anonymous"))
		 (:script :attrs (list :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js"
				       :integrity "sha384-wHAiFfRlMFy6i5SRaxvfOCifBUQy1xHdJ/yoi7FRNXMRBu5WHdZYu1hA6ZOblgut"
				       :crossorigin "anonymous"))
		 (:script :attrs (emit-bootstrap.min.js))))))))
      (let ((redirect-url
	      (format nil "~A/protocol/openid-connect/auth?client_id=~A&redirect_uri=~A/get-api-key&response_type=code&scope=openid%20profile%20email"
		      *keycloak-oidc-realm-redirect-uri*
		      *keycloak-oidc-client-id*
		      *server-uri*)))
	(log:info redirect-url)
	(hunchentoot:redirect redirect-url))))

(defun do-evaluate ()
  (authorize)
  (handler-case
      (let ((json-string
	      (funcall (read-from-string "hunchentoot:raw-post-data") :force-text t)))
	(log:info "evaluate: '~A'" json-string)
	(let ((json (json:decode-json-from-string json-string)))
	  (let ((policy-name (cdr (assoc :POLICY json)))
		(player (cdr (assoc :ID json))))
	    (cond
	      ((null policy-name)
	       (error "POLICY missing"))
	      ((not (rlgl.util:valid-url? policy-name))
	       (error "POLICY not a valid URL"))
	      ((null player)
	       (error "ID missing"))
	      (t
	       (authorize-policy-bound-api-key policy-name)
	       (let* ((policy (make-policy policy-name))
		      (doc (read-document *storage-driver* (cdr (assoc :REF json))))
		      (filename (cdr (assoc :NAME json)))
		      (parser (or (recognize-report doc)
				  (when (str:ends-with? ".csv" filename)
				    (make-instance 'parser/csv))))
		      (tests (if parser
				 (parse-report parser doc)
				 (error "DOCUMENT not recognized"))))
		 (log:info "Evaluating '~A'" (cdr (assoc :REF json)))
		 (progn
		   (multiple-value-bind (red-or-green processed-results)
		       (apply-policy policy tests)
                     (log:info "Policy applied")
		     (let ((stream (make-string-output-stream)))
		       (render stream
                               (doctype parser)
                               (ironclad:byte-array-to-hex-string (ironclad:digest-sequence 'ironclad:sha3/256 doc))
                               (cdr (assoc :REF json)) processed-results
			       (title parser)
			       (commit-url-format policy))
		       (let* ((doc-oc (flexi-streams:string-to-octets (get-output-stream-string stream)))
                              (ref (store-document *storage-driver* doc-oc))
                              (doc-digest (ironclad:byte-array-to-hex-string (ironclad:digest-sequence 'ironclad:sha3/256 doc-oc))))
                         (let ((doc-digest-signature (make-string-signature doc-digest)))
 			   (rlgl.db:record-log *db* player (version policy) red-or-green ref doc-digest-signature)
                           (track-action "evaluate" :url (format nil "/doc?id=~A" ref))
                           (rekor-envelope doc-digest doc-digest-signature)
			   (format nil "~A: ~A/doc?id=~A (sha3/256: ~A)~%"
			  	   red-or-green
				   *server-uri*
				   ref
                                   doc-digest))))))))))))
    (error (c)
      (log:error "~A" c)
      (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
      (format nil "ERROR: ~A~%" c))))

(defun do-upload ()
  (authorize)
  (track-action "upload")
  (handler-case
      (let* ((fpath (car (cdr (car (hunchentoot:post-parameters*)))))
	     (doc (alexandria:read-file-into-byte-vector
		   (rlgl.util:make-absolute-pathname fpath))))
	(store-document *storage-driver* doc))
    (error (c)
      (log:error "~A" c)
      (format nil "Error storing document: ~A" c))))

(snooze:defroute pubkey (:get :text/plain)
  "Return the public key for this server."
  (track-action "pubkey")
  (alexandria:read-file-into-string *public-key-file* :external-format :latin-1))

(snooze:defroute doc (:get :text/html &key id)
  "Delete this eventually."
  (track-action "doc" :url (format nil "/doc?id=~A" id))
  (let ((want-sig? (let ((id-string (string id)))
                     (if (str:ends-with? ".SIG" id-string)
                       (setf id (str:substring 0 (- (length id-string) 4) id-string))
                       nil))))
    (let ((report
  	  (handler-case (flexi-streams:octets-to-string
  			 (read-document *storage-driver* id)
  			 :external-format :utf-8)
  	    (error (c)
	      (log:error "~A" c)
	      (if want-sig?
                  "Error: document does not exist."
                  (alexandria:read-file-into-string
	            (rlgl.util:make-absolute-pathname "missing-doc.html") :external-format :latin-1))))))
    (if want-sig?
	(rlgl.db:find-signature-by-report *db* id)
        (if (str:starts-with? "<" report)
   	  report
 	  (format nil "<html><pre>~A</pre></html>" report))))))

(snooze:defroute doc-html (:get :text/html &key id)
  (track-action "doc-html" :url (format nil "/doc-html?id=~A" id))
  (handler-case (flexi-streams:octets-to-string
                 (read-document *storage-driver* id)
                 :external-format :utf-8)
    (error (c)
      (log:error "~A" c)
      (alexandria:read-file-into-string
       (rlgl.util:make-absolute-pathname "missing-doc.html") :external-format :latin-1))))

(snooze:defroute doc-text (:get :text/plain &key id)
  (track-action "doc-text" :url (format nil "/doc-text?id=~A" id))
  (handler-case (flexi-streams:octets-to-string
                 (read-document *storage-driver* id)
                 :external-format :utf-8)
    (error (c)
      (log:error "~A" c)
      (alexandria:read-file-into-string
       (rlgl.util:make-absolute-pathname "missing-doc.html") :external-format :latin-1))))

(snooze:defroute doc-pdf (:get :application/pdf &key id)
  (track-action "doc-pdf" :url (format nil "/doc-pdf?id=~A" id))
  (setf (hunchentoot:content-type*) "application/pdf")
  (read-document *storage-driver* id))

;;; END ROUTE DEFINITIONS -----------------------------------------------------

;;; Render processed results to HTML

(defparameter *unknown-matcher*
  (make-policy-matcher :kind :unknown))

(defun render (stream doctype digest report-ref results title commit-url-format)
  ;; We need to sort the results in order FAIL, XFAIL, and PASS, but
  ;; preserve order otherwise.
  (let ((fail nil)
	(xfail nil)
	(pass nil)
	(unknown nil))
    (dolist (item results)
      (if (car item)
	  (let ((kind (kind (car item))))
	    (cond
	      ((eq kind :FAIL)
	       (setf fail (cons item fail)))
	      ((eq kind :XFAIL)
	       (setf xfail (cons item xfail)))
	      ((eq kind :PASS)
	       (setf pass (cons item pass)))
	      (t t))) ; FIXME: error
	  (setf unknown (cons (cons *unknown-matcher*
				    (cdr item))
			      unknown))))
    (setf results
	  (concatenate 'list
		       (reverse fail)
		       (reverse xfail)
		       (reverse pass)
		       (reverse unknown))))
  (let ((*html* stream))
    (with-html
	(:doctype)
      (:html
       (:head
	(:meta :charset "utf-8")
	(:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
	(:link :rel "apple-touch-icon" :sizes "180x180" :href "images/apple-touch-icon.png")
	(:link :rel "icon" :type "image/png" :sizes "32x32" :href "images/favicon-32x32.png")
	(:link :rel "icon" :type "image/png" :sizes "16x16" :href "images/favicon-16x16.png")
	(:link :rel "manifest" :href "images/site.webmanifest")
	(:link :rel "mask-icon" :href "images/safari-pinned-tab.svg")
	(:meta :name "msapplication-TileColor" :content "#da532c")
	(:meta :name "theme-color" :content "#ffffff")
	(:title "Red Light Green Light Report")
	(:link :rel "stylesheet" :href "css/rlgl.css")
	(:link :attrs (emit-bootstrap.min.css))
	(:script :src "https://cdnjs.cloudflare.com/ajax/libs/prefixfree/1.0.7/prefixfree.min.js"))
       (:body
	(:header
	 (:nav :class "navbar navbar-expand-md navbar-dark fixed-top bg-dark"
	       (:a :class "navbar-brand"
		   :href "https://github.com/atgreen/red-light-green-light" "Red Light Green Light")))
	(:main :role "main" :class "container"
	       (:div :class "row"
		     (:div :class "col"
			   (:div :style "width:100px"
				 (:div :class "rlgl-svg"))
			   (:h1 :class "mt-5" title)
			   (:a :href (format nil "~A/doc-~A?id=~A"
                                             *server-uri*
                                             doctype
                                             report-ref)
			       :target "_blank" (format nil "Original Report (sha3/256: ~A)" digest))
			   (:table :class "fold-table" :id "results"
				   (:tr (:th "RESULT") (:th "ID"))
				   (dolist (item results)
				     (let ((matcher (car item))
					   (alist (cdr item)))
				       (:tr :class "view" :class (kind matcher)
					    (:td (kind matcher))
					    (:td (:a :href (cdr (assoc :URL alist))
						     :target "_blank" (cdr (assoc :ID alist)))))
				       (:tr :class "fold"
					    (:td :colspan "2")
					    (:div :class "fold-content"
						  (when (and matcher
							     (not (eq (kind matcher) :unknown)))
						    (let ((log-lines (log-entry matcher)))
						      (:div :id "border"
							    (:a :href (format nil commit-url-format (githash matcher))
								:target "_blank"
								(:pre (str:trim (car log-lines))))
							    (:pre (str:trim (format nil "~{~A~%~}" (cdr log-lines)))))))
						  (:div :id "border"
							(:pre (cl-json-util:pretty-json (json:encode-json-to-string alist))))))))))))
	(:footer :class "page-footer font-small special-color-dark pt-4"
		 (:div :class "footer-copyright text-center py-3" "Generated on "
		       (simple-date-time:http-date (simple-date-time:now)) " by version" +rlgl-version+
		       "   //   (C) 2018-2021" (:a :href "https://linkedin.com/in/green" " Anthony Green"))))
       (:script :attrs (list :src "https://code.jquery.com/jquery-3.3.1.slim.min.js"
			     :integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
			     :crossorigin "anonymous"))
       (:script :attrs (list :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js"
			     :integrity "sha384-wHAiFfRlMFy6i5SRaxvfOCifBUQy1xHdJ/yoi7FRNXMRBu5WHdZYu1hA6ZOblgut"
			     :crossorigin "anonymous"))
       (:script :attrs (emit-bootstrap.min.js))
       (:script :attrs (list :src "js/index.js"))))))

;;; HTTP SERVER CONTROL: ------------------------------------------------------
(defparameter *handler* nil)

(defun rlgl-root ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))

(defparameter +rlgl-dispatch-table+
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
   (hunchentoot:create-folder-dispatcher-and-handler
    "/cli/" (fad:pathname-as-directory
             (make-pathname :name "cli"
                            :defaults (rlgl-root))))
   (hunchentoot:create-prefix-dispatcher "/upload" 'do-upload)
   (hunchentoot:create-prefix-dispatcher "/evaluate" 'do-evaluate)
   (snooze:make-hunchentoot-app)))

(defclass exposer-acceptor (prom.tbnl:exposer hunchentoot:acceptor)
  ())

(defclass application (hunchentoot:easy-acceptor)
  ((exposer :initarg :exposer :reader application-metrics-exposer)
   (mute-access-logs :initform t :initarg :mute-access-logs :reader mute-access-logs)
   (mute-messages-logs :initform t :initarg :mute-error-logs :reader mute-messages-logs)))

(defmacro start-server (&key (handler '*handler*) (port 8080))
  "Initialize an HTTP handler"
  `(progn
     (setf snooze:*catch-errors* :verbose)
     (setf *print-pretty* nil)
     (setf hunchentoot:*dispatch-table* +rlgl-dispatch-table+)
     (setf prom:*default-registry* *rlgl-registry*)
     (let ((exposer (make-instance 'exposer-acceptor :registry *rlgl-registry* :port 9101)))
       (log:info "About to start hunchentoot")
       (setf ,handler (hunchentoot:start (make-instance 'application
							:document-root #p"./"
							:port ,port
							:exposer exposer))))))

(defmacro stop-server (&key (handler '*handler*))
  "Shutdown the HTTP handler"
  `(hunchentoot:stop ,handler))

;;; END SERVER CONTROL --------------------------------------------------------

(defun initialize-policy-dir (dir)
  "Initialize the policy directory."
  (handler-case
      (truename (ensure-directories-exist dir))
    (error ()
      (log:error "Can't initialize policy directory ~A" dir)
      nil)))

(defun start-rlgl-server (&optional (sleep-forever? nil))
  "Start the web application and have the main thread sleep forever if
  SLEEP-FOREVER? is not NIL."
  (setf hunchentoot:*catch-errors-p* t)
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)

  ;; We do lots of string conversion with flexi-streams.  In the case
  ;; where there are character encoding errors, replace the bad
  ;; character with '?'.
  (setf flexi-streams:*substitution-char* #\?)

  ;; We never git commit, but git will complain even when cloning if
  ;; we run in a container without a known user unless...
  (sb-posix:setenv "GIT_COMMITTER_NAME" "rlgl" 1)
  (sb-posix:setenv "GIT_COMMITTER_EMAIML" "rlgl@example.com" 1)

  (log:info "Starting rlgl-server version ~A" +rlgl-version+)

  ;; Read the built-in configuration settings.
  (setf *default-config* (cl-toml:parse +default-config-text+))
  (log:info +default-config-text+)

  ;; Read the user configuration settings.
  (setf *config*
	(if (fad:file-exists-p "/etc/rlgl/config.ini")
	    (cl-toml:parse
	     (alexandria:read-file-into-string "/etc/rlgl/config.ini"
					       :external-format :latin-1))
	    (make-hash-table)))

  (flet ((get-config-value (key)
	   (let ((value (or (gethash key *config*)
			    (gethash key *default-config*)
			    (error "config does not contain key '~A'" key))))
	     ;; Some of the users of these values are very strict
	     ;; when it comes to string types... I'm looking at you,
	     ;; SB-BSD-SOCKETS:GET-HOST-BY-NAME.
	     (if (subtypep (type-of value) 'vector)
		 (coerce value 'simple-string)
		 value))))

    (setf *server-uri* (or (uiop:getenv "RLGL_SERVER_URI")
			   (get-config-value "server-uri")))
    (unless (rlgl.util:valid-url? *server-uri*)
      (error "server-uri is not valid URL: ~A" *server-uri*))

    ;; Only read the matomo config values if the matomo URI is set.
    (when (or (uiop:getenv "MATOMO_URI") (gethash "matomo-uri" *config*))
      (setf *matomo-uri*
            (or (uiop:getenv "MATOMO_URI")
                (get-config-value "matomo-uri")))
      (setf *matomo-idsite*
            (or (uiop:getenv "MATOMO_IDSITE")
                (get-config-value "matomo-idsite")))
      (setf *matomo-token-auth*
            (or (uiop:getenv "MATOMO_TOKEN_AUTH")
                (get-config-value "matomo-token-auth"))))

    (setf *public-key-file*
	  (or (uiop:getenv "PUBLIC_KEY_FILE")
	      (get-config-value "public-key-file")))
    (setf *private-key-file*
	  (or (uiop:getenv "PRIVATE_KEY_FILE")
	      (get-config-value "private-key-file")))

    (setf *keycloak-oidc-client-id*
	  (or (uiop:getenv "KEYCLOAK_OIDC_CLIENT_ID")
	      (get-config-value "keycloak-oidc-client-id")))
    (setf *keycloak-oidc-client-secret*
	  (or (uiop:getenv "KEYLOAK_OIDC_CLIENT_SECRET")
	      (get-config-value "keycloak-oidc-client-secret")))
    (setf *keycloak-oidc-realm-redirect-uri*
	  (or (uiop:getenv "KEYLOAK_OIDC_REALM_REDIRECT_URI")
	      (get-config-value "keycloak-oidc-realm-redirect-uri")))
    (setf *keycloak-oidc-realm-uri*
	  (or (uiop:getenv "KEYLOAK_OIDC_REALM_URI")
	      (get-config-value "keycloak-oidc-realm-uri")))


    ;; Set up DB
    ;;
    (setf *db*
	  (let ((db (get-config-value "db")))
	    (alexandria:eswitch (db :test #'equal)
	      ("sqlite"
	       (make-instance 'rlgl.db:db/sqlite
			      :filename (get-config-value "sqlite-db-filename")))
	      ("postgresql"
	       (make-instance 'rlgl.db:db/postgresql
			      :password (or (uiop:getenv "POSTGRESQL_PASSWORD")
					    (get-config-value "postgresql-password"))
			      :host (get-config-value "postgresql-host")
			      :port (get-config-value "postgresql-port"))))))

    (log:info "About to initialize storage")

    ;; Define the storage backend.
    ;;
    (setf *storage-driver*
	  (make-instance
	   (read-from-string
	    (str:concat "rlgl-server:storage/"
			(get-config-value "storage-driver")))))

    (log:info "About to initialize policy-dir")

    ;; This is the directory where we check out policies.  Make sure it
    ;; ends with a trailing '/'.
    ;;
    (setf policy:*policy-dir*
	  (let ((dir (get-config-value "policy-dir")))
	    (pathname
	     (if (str:ends-with? "/" dir)
		 dir
		 (str:concat dir "/")))))

    ;; There may be a test api key defined in the config file.
    ;; Injest it if so...
    (handler-case
	(let ((test-api-key (get-config-value "test-api-key")))
	  (when test-api-key
	    (rlgl.db:register-test-api-key *db* test-api-key)))
      (error ()
	nil)))

  (unless (initialize-policy-dir *policy-dir*)
    (sb-ext:quit))

  (log:info "About to initialize metrics")

  (initialize-metrics)

  (log:info "About to start server")

  (thread-pool:start-pool *thread-pool*)

  (let ((srvr (start-server)))
    ;; If SLEEP-FOREVER? is NIL, then exit right away.  This is used by the
    ;; testsuite.
    (log:info "About to enter sleep loop")
    (when sleep-forever?
      (loop
	 (sleep 3000)))
    srvr))

(defun stop-rlgl-server ()
  "Stop the web application."
  (thread-pool:stop-pool *thread-pool*)
  (stop-server))

(defmethod hunchentoot:start ((app application))
  (hunchentoot:start (application-metrics-exposer app))
  (call-next-method))

(defmethod hunchentoot:stop ((app application) &key soft)
  (call-next-method)
  (hunchentoot:stop (application-metrics-exposer app) :soft soft))

(defmethod hunchentoot:acceptor-dispatch-request ((app application) request)
  (let ((labels (list (string-downcase (string (hunchentoot:request-method request)))
		      "rlgl_app")))
    (prom:counter.inc *http-requests-counter* :labels labels)
    (prom:histogram.time
     (prom:get-metric *http-request-duration* labels)
     (call-next-method))))
