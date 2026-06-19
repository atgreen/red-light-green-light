;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL; Base: 10 -*-
;;;
;;; Copyright (C) 2018-2025  Anthony Green <green@moxielogic.com>
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

;; Top level for the rlgl client-side tool.

(markup:enable-reader)

(in-package #:rlgl)

;; ----------------------------------------------------------------------------
;; Version.  Pull from git when available, otherwise fall back.

;; +RLGL-VERSION+ is the base version from rlgl.asd at load time, and is
;; updated to include git information (tag/hash, "+dirty" suffix) when the
;; standalone binary is built via program-op.
(version-string:define-version-parameter +rlgl-version+ :rlgl)

;; ----------------------------------------------------------------------------
;; Policy checkout directory.  Policies are git repositories that we
;; clone/pull into a local cache.

(defun ensure-policy-dir ()
  "Initialize POLICY:*POLICY-DIR* (where policy repos are cloned).  Resolve to
an OS-native absolute path so git receives a drive-qualified directory on
Windows (uiop:xdg-cache-home there yields a drive-less path that git
misresolves).  LOCALAPPDATA is only set on Windows, so this is a no-op
elsewhere."
  (let* ((base (or (uiop:getenv "RLGL_POLICY_DIR")
                   (let ((localappdata (uiop:getenv "LOCALAPPDATA")))
                     (and localappdata
                          (merge-pathnames "rlgl/policies/"
                                           (uiop:ensure-directory-pathname localappdata))))
                   (merge-pathnames "rlgl/policies/" (uiop:xdg-cache-home))))
         (dir (uiop:ensure-directory-pathname base)))
    (ensure-directories-exist dir)
    (setf policy:*policy-dir* (uiop:native-namestring dir))))

;; ----------------------------------------------------------------------------
;; Self-contained report assets.  The CSS (with the logo inlined as a
;; data URI) and the fold-table javascript are baked into the binary at
;; compile time so that generated reports are fully portable.

(defparameter +rlgl-css+
  #.(let* ((dir (directory-namestring
                 (or *compile-file-truename* *load-truename*
                     *default-pathname-defaults*)))
           (css (uiop:read-file-string (merge-pathnames "css/rlgl.css" dir)))
           (svg (uiop:read-file-string (merge-pathnames "images/rlgl.svg" dir))))
      (str:replace-all "url(../images/rlgl.svg)"
                       (concatenate 'string
                                    "url(\"data:image/svg+xml,"
                                    (quri:url-encode svg)
                                    "\")")
                       css)))

(defparameter +rlgl-index-js+
  #.(uiop:read-file-string
     (merge-pathnames "js/index.js"
                      (directory-namestring
                       (or *compile-file-truename* *load-truename*
                           *default-pathname-defaults*)))))

;; ----------------------------------------------------------------------------
;; Report recognition.  Recognition is done entirely in Lisp (no external
;; shell/grep), so the binary is self-contained and portable, including
;; native Windows.  Each recognizer is a predicate over the decoded document;
;; the first match (in order) wins.  This mirrors the historical recog.d
;; shell recognizers.

(defun %doc-string (doc)
  "Decode the byte vector DOC into a string for pattern matching, mapping
each byte 1:1 (latin-1) so binary input never signals."
  (flexi-streams:octets-to-string doc :external-format :latin-1))

(defun %first-lines (string n)
  "Return the first N lines of STRING as a single string."
  (let ((lines (uiop:split-string string :separator '(#\Newline))))
    (format nil "~{~A~^~%~}" (subseq lines 0 (min n (length lines))))))

(defparameter +report-recognizers+
  ;; (parser-class . predicate-over-the-decoded-document-string)
  (list
   (cons 'rlgl-parsers:parser/anchore
         (lambda (s) (search "imageDigest" (%first-lines s 3))))
   (cons 'rlgl-parsers:parser/aqua
         (lambda (s) (search "aqua_logo_" s)))
   (cons 'rlgl-parsers:parser/clair
         (lambda (s) (and (search "unapproved" s) (search "vulnerabilities" s))))
   (cons 'rlgl-parsers:parser/dejagnu
         (lambda (s) (and (cl-ppcre:scan "Test [Rr]un [Bb]y " s)
                          (search "Schedule of variations" s))))
   (cons 'rlgl-parsers:parser/junit
         (lambda (s) (and (search "xml" s)
                          (search "testsuite" s)
                          (search "testcase" s))))
   (cons 'rlgl-parsers:parser/oscap-oval
         (lambda (s) (search "<title>OVAL Results</title>" s)))
   (cons 'rlgl-parsers:parser/popeye
         (lambda (s) (search "<title>Popeye Sanitizer Report</title>" s)))
   (cons 'rlgl-parsers:parser/oscap-xccdf
         (lambda (s) (search "OpenSCAP Evaluation Report</title>" s)))
   ;; Tripwire PDF: a PDF whose report title is "PolicyReport".  (The full
   ;; parser still relies on external PDF tooling on Unix.)
   (cons 'rlgl-parsers:parser/tripwire-pdf
         (lambda (s) (and (str:starts-with? "%PDF" s)
                          (search "PolicyReport" s)))))
  "Ordered list of (PARSER-CLASS . PREDICATE) recognizers.")

(defun recognize-report (doc)
  "Try to recognize the report type in the byte vector DOC.  If we
recognize it, return an RLGL-PARSERS parser object, NIL otherwise."
  (let* ((s (%doc-string doc))
         (entry (find-if (lambda (e) (funcall (cdr e) s)) +report-recognizers+)))
    (when entry
      (make-instance (car entry)))))

;; ----------------------------------------------------------------------------
;; HTML rendering.

(markup:deftag page-template (children &key title)
   <html>
     <head>
       <meta charset="utf-8" />
       <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
       <meta name="theme-color" content="#ffffff" />
       <title>,(progn title)</title>
       <style>,(markup:unescaped +rlgl-css+)</style>
       <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"
	     integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC"
	     crossorigin="anonymous" />
       <script src="https://cdnjs.cloudflare.com/ajax/libs/prefixfree/1.0.7/prefixfree.min.js" ></script>
     </head>
     <header>
       <nav class="navbar navbar-expand-md navbar-dark fixed-top bg-dark">
         <div class="container-fluid" style="margin-left: 1rem; margin-right: 1rem;">
           <a class="navbar-brand" href="https://github.com/atgreen/red-light-green-light">Red Light Green Light</a>
         </div>
       </nav>
     </header>
     <body>
       <main class="container" role="main">
         <div class="row" >
           <div class="col" >
             ,@(progn children)
             <hr/>
             Red Light Green Light was written by Anthony Green <a href="mailto:green@moxielogic.com" >&lt;green@moxielogic.com&gt</a>
             and is available in source form under the terms of the AGPLv3 license from
             <a href="https://github.com/atgreen/red-light-green-light" > https://github.com/atgreen/red-light-green-light </a>.
           </div>
         </div>
       </main>
     </body>
     <footer class="page-footer font-small
                    special-color-dark pt-4">
       <div class="footer-copyright
                   text-center py-3">Version ,(progn +rlgl-version+) // (C) 2018-2025<a href="https://linkedin.com/in/green" > Anthony Green</a></div>
     <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js"
             integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
             crossorigin="anonymous" ></script>
     <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js"
	     integrity="sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM"
             crossorigin="anonymous" ></script>
             <script>,(markup:unescaped +rlgl-index-js+)</script>
      </footer>
   </html>)

(defparameter *unknown-matcher*
  (make-policy-matcher :kind :unknown))

(defun render (stream original-report-href digest results title commit-url-format columns labels)
  "Render the processed RESULTS to STREAM as a self-contained HTML report."
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
		                   (reverse unknown)))
    (let ((report-columns (if columns columns '(:RESULT :ID))))
          (markup:write-html-to-stream
          <page-template title="Red Light Green Light" >
            <div class="row" >
              <div class="col" >
                <div style="width:100px" >
                  <div class="rlgl-svg" />
                </div>
                <h1 class="mt-5" > ,(format nil "~A" title) </h1>
            <a href=(progn original-report-href) target="_blank" >
              ,(format nil "Original Report (sha3-256: ~A)" digest)
            </a>
            ,(when labels
               <table>
                 <markup:merge-tag>
                 ,@(mapcar (lambda (label)
                             <tr><td> ,(json:lisp-to-camel-case (string (car label))) </td><td> ,(cdr label) </td></tr> )
                             labels)
                 </markup:merge-tag>
               </table> )
            <table class="fold-table" id="results" >
              <tr> ,@(mapcar (lambda (c)
                               <th> ,(string c) </th> )
                             report-columns)
              </tr>
              ,@(mapcar (lambda (item)
            	          (let ((matcher (car item))
			                (alist (cdr item)))
                            <markup:merge-tag>
	                    <tr class=(format nil "view ~A" (string-downcase (kind matcher))) >
                              ,@(mapcar (lambda (c)
                                          (case c
                                            (:RESULT <td> ,(kind matcher) </td> )
                                            (:ID <td> <a href=(cdr (assoc :URL alist)) target="_blank"> ,(cdr (assoc :ID alist)) </a> </td> )
                                            (:URL " ")
                                            (otherwise <td> ,(cdr (assoc c alist)) </td> )))
                                        report-columns)
                            </tr>
	                    <tr class="fold">
		              <td colspan=(format nil "~A" (length report-columns)) >
		                <div class="fold-content" >
		                  ,(when (and matcher (not (eq (kind matcher) :unknown)))
		                  (let ((log-lines (log-entry matcher)))
  		                  <div id="border" >
                                    <a href=(format nil commit-url-format (githash matcher)) target="_blank" >
		                      <pre> ,(str:trim (car log-lines)) </pre>
                                    </a>
		                    <pre> ,(str:trim (format nil "~{~A~%~}" (cdr log-lines))) </pre>
                                  </div> ))
  	                          <div id="border" >
		                    <pre> ,(jsown:pretty-json (json:encode-json-to-string alist)) </pre>
                                  </div>
                                </div>
                             </td>
              </tr></markup:merge-tag> ))
                          results )
            </table>
          </div>
        </div>
      </page-template>
      stream))))

;; ----------------------------------------------------------------------------
;; Core pipeline.

(defun %analyze (report-path policy-url labels)
  "Recognize REPORT-PATH, parse it, clone/pull POLICY-URL, and apply the
policy.  Returns (values RED-OR-GREEN PROCESSED-RESULTS PARSER DOC POLICY)."
  (ensure-policy-dir)
  (let* ((doc (alexandria:read-file-into-byte-vector report-path))
         (filename (file-namestring (pathname report-path)))
         (parser (or (recognize-report doc)
                     (when (str:ends-with? ".csv" filename)
                       (make-instance 'rlgl-parsers:parser/csv))))
         (tests (if parser
                    (rlgl-parsers:parse-report parser doc labels)
                    (error "Report not recognized: ~A" filename)))
         (policy (make-policy policy-url)))
    (multiple-value-bind (red-or-green processed)
        (apply-policy policy tests)
      (values red-or-green processed parser doc policy))))

(defun evaluate (report-path policy-url &key labels title output)
  "Evaluate the local report at REPORT-PATH against the policy at the git
URL POLICY-URL.  Writes a self-contained HTML report and a copy of the
original report alongside it.  Returns (values RED-OR-GREEN OUTPUT-PATH)."
  (multiple-value-bind (red-or-green processed parser doc policy)
      (%analyze report-path policy-url labels)
    (let* ((out (pathname (or output "rlgl-report.html")))
           (orig (make-pathname
                  :name (concatenate 'string
                                     (or (pathname-name out) "rlgl-report")
                                     "-original")
                  :type (or (pathname-type (pathname report-path))
                            (string-downcase (string (rlgl-parsers:doctype parser))))
                  :defaults out))
           (digest (ironclad:byte-array-to-hex-string
                    (ironclad:digest-sequence 'ironclad:sha3/256 doc))))
      ;; Copy the original report next to the rendered one and link to it.
      (uiop:copy-file (pathname report-path) orig)
      (with-open-file (stream out :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
        (render stream
                (file-namestring orig)
                digest
                processed
                (or title (rlgl-parsers:title parser))
                (commit-url-format policy)
                (rlgl-parsers:columns parser)
                labels))
      (values red-or-green (truename out)))))

(defun generate-baseline (report-path policy-url &key labels)
  "Evaluate REPORT-PATH against POLICY-URL and return, as a string, an
XFAIL regression policy that excuses every currently-failing result.
Each line is a JSON matcher suitable for an rlgl policy XFAIL file."
  (multiple-value-bind (red-or-green processed)
      (%analyze report-path policy-url labels)
    (declare (ignore red-or-green))
    (with-output-to-string (s)
      (dolist (item processed)
        (let ((matcher (car item))
              (alist (cdr item)))
          (when (and matcher (eq (kind matcher) :FAIL))
            (format s "~A~%" (json:encode-json-to-string alist))))))))

;; ----------------------------------------------------------------------------
;; Command line interface.

(defun parse-labels (label-strings)
  "Turn a list of \"KEY=VALUE\" strings into an alist of (:KEY . \"VALUE\")."
  (mapcar (lambda (s)
            (let ((pos (position #\= s)))
              (if pos
                  (cons (intern (string-upcase (subseq s 0 pos)) :keyword)
                        (subseq s (1+ pos)))
                  (cons (intern (string-upcase s) :keyword) ""))))
          label-strings))

(defun report-argument (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (cond
      ((null args) (error "Missing report file argument"))
      ((cdr args)  (error "Too many arguments; expected a single report file"))
      (t (first args)))))

(defun evaluate/options ()
  (list
   (clingon:make-option :string :short-name #\p :long-name "policy"
                        :description "policy git URL (required)" :key :policy)
   (clingon:make-option :list :short-name #\l :long-name "label"
                        :description "set a report label KEY=VALUE" :key :label)
   (clingon:make-option :string :short-name #\t :long-name "title"
                        :description "report title" :key :title)
   (clingon:make-option :filepath :short-name #\o :long-name "output"
                        :description "output HTML report file"
                        :initial-value "rlgl-report.html" :key :output)))

(defun evaluate/handler (cmd)
  (let ((policy (clingon:getopt cmd :policy))
        (labels (parse-labels (clingon:getopt cmd :label)))
        (title (clingon:getopt cmd :title))
        (output (clingon:getopt cmd :output))
        (report (report-argument cmd)))
    (unless policy
      (error "Missing required --policy option"))
    (multiple-value-bind (red-or-green out-path)
        (evaluate report policy :labels labels :title title :output output)
      (format t "~A~%" (string red-or-green))
      (format t "Report written to ~A~%" out-path)
      (uiop:quit (if (eq red-or-green :GREEN) 0 1)))))

(defun evaluate/command ()
  (clingon:make-command
   :name "evaluate"
   :aliases '("e")
   :description "evaluate a test report against a policy"
   :usage "--policy URL [options] REPORT"
   :options (evaluate/options)
   :handler #'evaluate/handler))

(defun baseline/options ()
  (list
   (clingon:make-option :string :short-name #\p :long-name "policy"
                        :description "policy git URL (required)" :key :policy)
   (clingon:make-option :list :short-name #\l :long-name "label"
                        :description "set a report label KEY=VALUE" :key :label)))

(defun baseline/handler (cmd)
  (let ((policy (clingon:getopt cmd :policy))
        (labels (parse-labels (clingon:getopt cmd :label)))
        (report (report-argument cmd)))
    (unless policy
      (error "Missing required --policy option"))
    (write-string (generate-baseline report policy :labels labels))
    (uiop:quit 0)))

(defun baseline/command ()
  (clingon:make-command
   :name "baseline"
   :aliases '("b")
   :description "generate a baseline XFAIL regression policy from a report"
   :usage "--policy URL [options] REPORT"
   :options (baseline/options)
   :handler #'baseline/handler))

(defun top/handler (cmd)
  ;; No subcommand given: show usage.
  (clingon:print-usage-and-exit cmd t))

(defun make-app ()
  (clingon:make-command
   :name "rlgl"
   :version +rlgl-version+
   :description "Red Light Green Light - a git-centric policy enforcement tool"
   :authors '("Anthony Green <green@moxielogic.com>")
   :license "AGPL3"
   :handler #'top/handler
   :sub-commands (list (evaluate/command)
                       (baseline/command))))

(defun main ()
  ;; Keep the engine's internal logging quiet for a command-line tool.
  (log:config :warn)
  (handler-case
      (clingon:run (make-app))
    (policy:policy-repo-error (c)
      (format *error-output* "Error: ~A~%" (policy:policy-repo-error-description c))
      (uiop:quit 1))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 1))))
