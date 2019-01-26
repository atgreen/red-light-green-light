# CL-DBI - Database independent interface for Common Lisp

[![Build Status](https://travis-ci.org/fukamachi/cl-dbi.svg?branch=master)](https://travis-ci.org/fukamachi/cl-dbi)

## Usage

### Connecting and executing a query

```common-lisp
(defvar *connection*
  (dbi:connect :mysql
               :database-name "test"
               :username "nobody"
               :password "1234"))

(let* ((query (dbi:prepare *connection*
                           "SELECT * FROM somewhere WHERE flag = ? OR updated_at > ?"))
       (result (dbi:execute query 0 "2011-11-01")))
  (loop for row = (dbi:fetch result)
     while row
     ;; process "row".
       ))
```

### Using `dbi:with-connection` to ensure connections are closed

```common-lisp
(dbi:with-connection (conn :sqlite3 :database-name "/home/fukamachi/test.db")
  (let* ((query (dbi:prepare conn "SELECT * FROM People"))
         (result (dbi:execute query)))
    (loop for row = (dbi:fetch result)
       while row
       do (format t "~A~%" row))))
```

## Description

CL-DBI provides the same interface for multiple SQL databases. You need not learn the API of each database.

This library is especially convenient when you want to use different databases in different environments. For example, you may use MySQL as a production database, but use SQLite3 on your development system. To switch database backends you need only change the arguments to `dbi:connect`.

## Databases

* SQLite3
* PostgreSQL
* MySQL

## Installation

This library is available on [Quicklisp](https://www.quicklisp.org/).

```common-lisp
CL-USER> (ql:quickload :cl-dbi)
To load "cl-dbi":
  Load 1 ASDF system:
    cl-dbi
; Loading "cl-dbi"

(:CL-DBI)
```

## API

### User-Level API

* connect [driver-name &amp; params] =&gt; &lt;dbi-connection&gt;
* connect-cached [driver-name &amp; params] =&gt; &lt;dbi-connection&gt;
* disconnect [&lt;dbi-connection&gt;] =&gt; T or NIL
* prepare [conn sql] =&gt; &lt;dbi-query&gt;
* execute [query &amp; params] =&gt; something
* fetch [result] =&gt; a row data as plist
* fetch-all [result] =&gt; a list of all row data
* do-sql [conn sql &amp; params]
* list-all-drivers [] =&gt; (&lt;dbi-driver&gt; ..)
* find-driver [driver-name] =&gt; &lt;dbi-driver&gt;
* with-transaction [conn]
* begin-transaction [conn]
* commit [conn]
* rollback [conn]
* ping [conn] =&gt; T or NIL
* row-count [conn] =&gt a number of rows modified by the last executed INSERT/UPDATE/DELETE
* with-connection [connection-variable-name &body body]

### Driver-Level API

* &lt;dbi-driver&gt;
* &lt;dbi-connection&gt;
* make-connection [driver params]
* disconnect [&lt;dbi-connection&gt;] =&gt; T or NIL
* prepare [conn sql] =&gt; &lt;dbi-query&gt;
* fetch-using-connection [conn result] =&gt; a row data as plist
* do-sql [conn sql &amp; params]
* execute-using-connection =&gt; something
* escape-sql =&gt; string
* begin-transaction [conn]
* commit [conn]
* rollback [conn]
* ping [conn] =&gt; T or NIL
* row-count [conn] =&gt a number of rows modified by the last executed INSERT/UPDATE/DELETE

## Creating a new driver

Create a subclass of &lt;dbi-driver&gt; and implement following methods.

* make-connection
* disconnect [&lt;dbi-connection&gt;] =&gt; T or NIL
* execute-using-connection

These methods can be overriden if needed.

* prepare
* fetch-using-connection
* do-sql
* escape-sql

## Dependencies

* cl-annot
* CL-Syntax
* SPLIT-SEQUENCE
* closer-mop

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.

