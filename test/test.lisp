;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
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

(in-package #:cl-user)
(defpackage #:test-rlgl
  (:use #:common-lisp #:prove #:matcher #:policy)
  (:export #:run))
(in-package #:test-rlgl)

(setf prove:*default-reporter* :fiveam)

(defparameter *junit-report* "test/sample-junit.xml")

(defun make-temp-policy (pass fail xfail)
  "Create a temporary git policy repository with the given PASS, FAIL,
and XFAIL file contents.  Return its path as a namestring."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames
               (format nil "rlgl-test-policy-~A/" (rlgl-util:random-base36-string))
               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (loop for (name . contents) in `(("PASS" . ,pass) ("FAIL" . ,fail) ("XFAIL" . ,xfail))
          do (with-open-file (s (merge-pathnames name dir)
                                :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
               (write-string contents s)
               (terpri s)))
    (flet ((git (&rest args)
             (uiop:run-program (list* "git" "-C" (namestring dir) args)
                               :output :string :error-output :string)))
      (git "init")
      (git "add" ".")
      (git "-c" "user.email=test@example.com" "-c" "user.name=Test"
           "commit" "-m" "test policy"))
    (namestring dir)))

(defun run ()
  (plan nil)

  (let ((green-policy (make-temp-policy "{ \"report\": \"junit\" }" "# none" "# none"))
        (red-policy   (make-temp-policy "# none" "{ \"report\": \"junit\" }" "# none"))
        (out (namestring (merge-pathnames "rlgl-test-report.html"
                                          (uiop:temporary-directory)))))

    (subtest "GREEN evaluation"
      (multiple-value-bind (colour path)
          (rlgl:evaluate *junit-report* green-policy
                         :title "Test" :output out)
        (is colour :GREEN "everything passes -> GREEN")
        (ok (probe-file path) "an HTML report is written")
        (ok (search "<!DOCTYPE html>"
                    (uiop:read-file-string path))
            "report is HTML")))

    (subtest "RED evaluation"
      (multiple-value-bind (colour path)
          (rlgl:evaluate *junit-report* red-policy
                         :title "Test" :output out)
        (is colour :RED "matching FAIL pattern -> RED")
        (ok (probe-file path) "an HTML report is written")))

    (subtest "baseline generation"
      (let ((baseline (rlgl:generate-baseline *junit-report* red-policy)))
        (ok (> (length baseline) 0) "baseline policy is non-empty")
        (ok (search "junit" baseline) "baseline contains junit matchers"))))

  (finalize))
