[![Build Status](https://travis-ci.org/atgreen/red-light-green-light.svg?branch=master)](https://travis-ci.org/atgreen/red-light-green-light)
[![Coverage Status](https://coveralls.io/repos/github/atgreen/red-light-green-light/badge.svg)](https://coveralls.io/github/atgreen/red-light-green-light)

**This is an experimental Work In Progress**
---

Red Light Green Light
===============================

**Red Light Green Light** is a git-centric policy management and
  enforcement tool designed to accelerate your CI/CD pipelines.


Problem Statement
----------------

Modern DevOps pipelines require Quality Gating mechanisms in order to
fully automate the promotion of software artifacts from dev all the
way through to prod.  These Quality Gates are responsible for deciding
whether or not a deployable artifact (config files, software builds,
container images, VM images, etc) should proceed to the next stage of
a CI/CD pipeline.

Many tools are available to evaluate the quality of deployable
artifacts, including container image scanners, unit test harnesses,
config file linters, etc.  But dealing with multiple quality testing
tools introduces problems:

 - bespoke gating mechanisms must be created to evaluate test results
 - different tools require different exception processes and policy management
 - no centralized, auditable policy management
 - policies are locked within proprietary tools

Policy Driven Quality Gates
------------------------------

The main idea behind Red Light Green Light is to decouple the process
of evaluating test results away from the underlying testing tools
themselves, in a way that is:

 - centrally managed
 - version controlled
 - auditable
 - customizeable
 - protected with authentication/authorization mechanisms

The goal of all of this is to enable auditors to easily answer the
following questions as they relate to any artifact promoted through a
CI/CD pipeline:

 - who presented test results for evaluation?
 - what were those test results?
 - what policies were they evaluated against?
 - who defined the policies and when?

The Red Light Green Light service is invoked via the `rlgl`
command-line tool, typically within some other pipeline
automation framework, such as a [jenkins](https://jenkins.io)
pipeline.  Here's an example workflow:

- First, we must log into our Red Light Green Light server with `rlgl`
cli tool the like so:
```
$ rlgl login -u USERNAME -p PASSWORD http://rlgl-server.example.com
```

- Each deployable artifact is given a Player ID.  The Player ID is
  what flows down the pipeline along with the various build/deploy
  artifacts.  They would be attached as artifact metadata.

```
$ ID=$(rlgl start)
```

- As the pipeline proceeds, test results are generated (scans, unit
  tests, etc).  For each test report generated, `rlgl` evaluates the
  report against the stated git-hosted policy, resulting in a **Red
  Light**, meaning stop the pipeline, or **Green Light**, meaning
  proceed with the pipeline.  It also produces a URL that links to a
  report showing annotated evaluation results.  Annotations, include,
  for example, the git logs for policies defining exceptions resulting
  in green lights.

```shell
$ rlgl evaluate --policy https://git.example.com/policy/dev.git --id $ID my-test-report.html
GREEN: http://rlgl-server.example.com/RLGL-BC7DB3F
```

```shell
$ rlgl evaluate --policy https://git.example.com/policy/prod.git --id $ID oval-scan.xml
RED: http://rlgl-server.example.com/RLGL-1CF5B3A
```
   
```shell
$ rlgl evaluate --policy https://git.example.com/policy/rel.git --id $ID gcc.log
GREEN: http://rlgl-server.example.com/RLGL-AFC7DB2
```

Standard exit codes make it easy to integrate `rlgl` into your CI/CD
pipeline scripts.  `GREEN` lights have an exit code of 0.  `RED`
lights have an exit code of 1.  Any other exit code is an error.

That's it!   The client side is very easy.   

The server side, where policy is evaluated, is where the magic is.

The first step is to identify the type of report we're evaluating and
convert it into a canonical form.  The canonical form is defined
simply as this: a JSON object.  No special schema is defined.

Policies are maintained in git repos, and consist of three plain text
files: `XFAIL`, `FAIL`, and `PASS`.  Each of these files contains a
list of JSON matching expressions to match again the canonical test
results.  They are evaluated this order: `XFAIL`, `FAIL`, `PASS`.

`XFAIL` contains matchers for test results we are expecting to fail and
allowing to pass anyway.  These are your exceptions.  Any matching
JSON objects are removed from the test results before processing with
FAIL.

`FAIL` contains matchers for tests results that are definitely failures.
They are removed from the test results before processing with `PASS`.

`PASS` contains matchers for known test passes.  These are removed from
the test results.

Any remaining entries in the test results are recorded as `UNKNOWN`.
`rlgl` interprets these as failures, but they are reported as
`UNKNOWN` in order aim for 100% coverage of the `PASS`/`FAIL` scans.

The `XFAIL`, `FAIL`, `PASS` files are maintained in a git
repo. Changing policy requires modifying the policy in git, which is
logged and auditable.

In addition to this simple test evaluation service, the server can
report on which policies have received green lights for each Player
ID, and records all test documents for archive and audit purposes.

All reports submitted to and generated by Red Light Green Light are
currently archived forever.


Policy in Detail
---------------

A `rlgl` policy consists of three separate files in a git repo:
`XFAIL`, `FAIL` and `PASS`. Each file contains JSON matchmaking
expressions, comments and blank lines.  Comments are lines starting
with the characters `#` or `;`.  The matchmaking expressions are
single-line JSON objects.

For example, to mark a CVE failure as an exception, we add the
following to our `XFAIL` file:

    # Ignore this failure in our container images
    { "result": "FAIL", "id": "CVE-2014-4043" }

Each JSON field string must match the corresponding string in the test
result object exactly.  There are two special forms of string values.
Strings starting with "^" are interpreted as regular expressions, and
strings of the form "NUMBER..NUMBER" are interpreted as a numeric
range.

So, for example, to ignore all CVE vulnerabilities from 2013 with a
score of less than 7 we add the following to our `XFAIL` file:

    # Ignore everything but the most critical CVEs from 2013.
    { "result": "FAIL", "id": "^CVE-2013.*", "score": "0..6" }

Every element of the matchmaking expression must match the test result
in order to qualify as a match.

JSON matchmaking expressions cannot span more than one line of text.
This is required in order to attribute policy changes to individuals
via `git blame`.  These changelogs are available through the `rlgl`
reports generated at evaluation time.

Report Parsers
---------------

Currently supported report parsers include:

* [OpenSCAP](https://www.open-scap.org/) OVAL scan reports
* [JUnit](https://junit.org/junit5/) XML results report
* [AquaSec](https://github.com/aquasecurity/microscanner) container microscanner reports

A generic CSV parser is in the works, allowing you to implement policy
on any arbitrary metric (file size, performance results, etc).

While the `rlgl` command-line tool is written in
[Go](https://golang.org/), the server side is written in [Common
Lisp](https://github.com/container-lisp), and adding additional report
types requires modifying the `rlgl-server` lisp code.  External parser
support is planned, allowing you to invoke report parsers through a
simple API.


Author and License
-------------------

Red Light Green Light was written by [Anthony
Green](https://github.com/atgreen), and is distributed under the terms
of the GNU GPLv3.  See
[COPYING3](https://raw.githubusercontent.com/atgreen/red-light-green-light/master/COPYING3)
for details.
