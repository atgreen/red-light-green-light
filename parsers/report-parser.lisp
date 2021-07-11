(in-package :rlgl-parsers)

(defclass report-parser ()
  ((name :initarg :name :reader name)
   (title :initarg :title :reader title)
   (doctype :initarg :doctype :reader doctype)))
