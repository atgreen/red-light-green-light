;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-PARSERS; Base: 10 -*-
;;;
;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>
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

(in-package :rlgl-parsers)

;;; MVP Popeye HTML Results report parser

(defclass parser/popeye (report-parser)
  ()
  (:default-initargs
   :title  "Popeye Scan Report"
   :doctype "html"
   :columns '(:RESULT :SECTION :OUTCOME :ID :LEVEL)))

(defmethod parse-report ((parser parser/popeye) doc)
  (let ((pdoc (plump:parse (flexi-streams:make-flexi-stream
			    (flexi-streams:make-in-memory-input-stream doc)
			    :external-format :utf-8)))
	(tests-fail (list)))
    (let ((sections (lquery:$ pdoc "div.section")))
      (loop for section across sections do
        (let ((section-title
                (car (split-sequence:split-sequence
                      #\space
                      (str:trim
                       (aref (lquery:$ section "div.section-title" (text)) 0))))))
          (let ((outcomes (lquery:$ section "ul.outcome > li")))
            (loop for outcome across outcomes do
              (let ((outcome-name (str:trim (aref (lquery:$ outcome "div.outcome" (text)) 0))))
                (let ((issues (lquery:$ outcome "ul.issues > li > span")))
                  (map nil (lambda (issue)
                             (let ((level (str:s-last (aref (lquery:$ issue (attr "class")) 0)))
                                   (text (str:trim (aref (lquery:$ issue (text)) 0))))
                               (when (> (parse-integer level) 0)
                                 (setf tests-fail
                                       (cons
                                        (json:decode-json-from-string
                                         (format nil "{ \"report\": \"popeye\", \"result\": \"FAIL\", \"section\": \"~A\", \"outcome\": \"~A\", \"id\": \"~A\", \"level\": ~S }"
                                                 section-title
                                                 outcome-name
                                                 (ppcre:regex-replace-all "\"" text "\\\"" )
                                                 level))
                                        tests-fail)))))
                       issues))))))))
    (reverse tests-fail)))
