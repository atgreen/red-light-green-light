;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2020  Anthony Green <green@moxielogic.com>
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

(in-package :rlgl-server)

;;; MVP Tripwire Results report parser

;; ----------------------------------------------------------------------------

(defclass parser/tripwire-pdf (report-parser)
  ()
  (:default-initargs
   :title  "Tripwire Scan Report"
   :doctype "pdf"))

(defparameter +newline+ (format nil "~A" #\newline))

(defun find-next (pos str doc)
  (let ((p-start (search str doc :start2 pos)))
    (when p-start
      (let* ((p-start (+ p-start (length str)))
             (p-end (search +newline+ doc :start2 p-start)))
        (when p-end
          (values (subseq doc p-start p-end) p-end))))))

(defun find-prev (pos str doc)
  (let ((p-start (search str doc :end2 pos :from-end t)))
    (when p-start
      (let* ((p-start (+ p-start (length str)))
             (p-end (search +newline+ doc :start2 p-start)))
        (when p-end
          (values (subseq doc p-start p-end) p-end))))))

(defun find-next-test (pos doc)
  (multiple-value-bind (test-name pos)
      (find-next pos "Test Name: " doc)))

(defun parse-tripwire-text (text)
  (let ((tests (list))
        (position 0))
    (multiple-value-bind (policy-name ppos)
        (find-next position "Policy Name: " text)
      (multiple-value-bind (test-name tpos)
          (find-next ppos "Test Name: " text)
        (loop
          while test-name
          do (multiple-value-bind (rule-name rpos)
                 (find-prev tpos "Rule Name: " text)
               (setf tests (cons (json:decode-json-from-string
                                  (format nil "{ \"report\": \"tripwire-pdf\", \"status\": ~S, \"policy\": ~S, \"rule\": ~S, \"id\": ~S }"
                                          (find-next tpos "Status: " text)
                                          (rlgl.util:escape-json-string policy-name)
                                          (rlgl.util:escape-json-string rule-name)
                                          (rlgl.util:escape-json-string test-name)))
                                 tests))
               (multiple-value-bind (next-test-name next-tpos)
                   (find-next (+ 1 tpos) "Test Name: " text)
                 (setf test-name next-test-name)
                 (setf tpos next-tpos))))))
    tests))

(defmethod parse-report ((parser parser/tripwire-pdf) doc)
  (let* ((base-filename (format nil "/tmp/temp-~A"
                                (generate-random-string)))
         (pdf-filename (concatenate 'string base-filename ".pdf"))
         (txt-filename (concatenate 'string base-filename ".txt")))
    (unwind-protect
         (progn
           (with-open-file (stream pdf-filename
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
             (write-sequence doc stream))
           (inferior-shell:run
            (format nil "pdftotext ~A ~A" pdf-filename txt-filename))
           (parse-tripwire-text (alexandria:read-file-into-string
                                  txt-filename :external-format :latin-1)))
      (uiop:delete-file-if-exists pdf-filename)
      (uiop:delete-file-if-exists txt-filename))))
