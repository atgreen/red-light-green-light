<img src="images/rlgl.svg.png" align="right" />

# Red Light Green Light
> A git-centric policy management and enforcement tool designed to accelerate your CI/CD pipelines

[![Build Status](https://github.com/atgreen/red-light-green-light/actions/workflows/build.yml/badge.svg)](https://github.com/atgreen/red-light-green-light/actions)

`rlgl` is a self-contained, client-side command-line tool.  It
evaluates a test report against a git-hosted policy entirely on your
local machine — there is no server, account, or network service to log
into.  Policies live in ordinary git repositories, and `rlgl` clones
them on demand.

For an example of real-world rlgl policy in action, check out the
policy used to validate unit test reports for
[libffi](https://github.com/libffi/libffi) on
[Github Actions](https://github.com/libffi/libffi/actions) builds:
[https://github.com/libffi/rlgl-policy](https://github.com/libffi/rlgl-policy).

Quick Start
------------

Generate an OpenSCAP report, or grab one from here:

    $ curl https://raw.githubusercontent.com/atgreen/red-light-green-light/master/test/report.html > report.html

Evaluate the report against a sample policy:

    $ rlgl evaluate --label id=sample-test --policy=https://github.com/atgreen/test-policy report.html
    GREEN
    Report written to /current/dir/rlgl-report.html

The `--label id=sample-test` option adds the `id=sample-test`
key/value pair to the report.  You can add any number of labels; they
become fields in the normalized test results and are available for
pattern matching in policy.

`rlgl` writes a self-contained HTML report (default
`rlgl-report.html`, override with `-o/--output`) alongside a copy of
the original report.  Open it in a browser to explore the annotated
evaluation results, complete with links to the git-hosted policy
commits responsible for each result.

To use a private repository, generate a personal access token at
github.com with appropriate private repo access, and reference your
policy repo like so:

    $ rlgl evaluate -l id=sample-test --policy=https://${TOKEN}@github.com/atgreen/test-policy report.html

Standard exit codes make it easy to integrate `rlgl` into your CI/CD
pipeline scripts.  `GREEN` lights have an exit code of 0.  `RED`
lights have an exit code of 1.  Any other exit code is an error.

Problem Statement
----------------

Modern DevOps pipelines require Quality Gating mechanisms in order to
fully automate the promotion of software artifacts from dev all the
way through to prod. These Quality Gates are responsible for deciding
whether a deployable artifact (config files, software builds,
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

 - version controlled
 - auditable
 - customizeable

The `rlgl` tool is typically invoked within some other pipeline
automation framework, such as a [jenkins](https://jenkins.io)
pipeline.  As the pipeline proceeds, test results are generated
(scans, unit tests, etc).  For each test report generated, `rlgl`
evaluates the report against the stated git-hosted policy, resulting
in a **Red Light**, meaning stop the pipeline, or **Green Light**,
meaning proceed with the pipeline.  It also produces an HTML report
showing annotated evaluation results.  Annotations include, for
example, the git logs for policies defining exceptions resulting in
green lights.

```shell
$ rlgl evaluate --policy https://git.example.com/policy/dev.git -l id=$ID my-test-report.html
GREEN
Report written to /current/dir/rlgl-report.html
```

```shell
$ rlgl evaluate --policy https://git.example.com/policy/prod.git -l id=$ID -o scan.html oval-scan.xml
RED
Report written to /current/dir/scan.html
```

How evaluation works
---------------------

The first step is to identify the type of report we're evaluating and
convert it into a normalized form.  The normalized form is defined
simply as this: a sequence of JSON objects, one for each testcase
result.  This object has two required fields:

* `result`: whose value is either `PASS` or `FAIL`.
* `id`: a descriptive ID for the testcase (e.g. CVE number).

One optional field is:

* `url`: a URL linking to contextual info for that `id`.

All labels passed on the `rlgl evaluate` command appear as fields in
the normalized output form.  The report parser may also add report
type-specific extra fields to the normalized output as they see fit.
This is useful for pattern matching, as described below.

Policies are maintained in git repos, and consist of three plain text
files: `XFAIL`, `FAIL`, and `PASS`.  Each of these files contains a
list of JSON matchmaking expressions to match against the canonical
test results.  They are evaluated in this order: `XFAIL`, `FAIL`, `PASS`.

`XFAIL` contains matchmakers for test results we are expecting to
fail, but allowing to pass anyway.  These are your exceptions.  Any
matching JSON objects are reported as green, and filtered out from the
list of test results to be processed by `FAIL`.

`FAIL` contains matchmakers for tests results that are definitely
failures.  They are reported as red, and filtered out from the test
results before processing with `PASS`.

`PASS` contains matchers for known test passes and reported as green.

Any remaining entries in the test results are recorded as `UNKNOWN`.
`rlgl` interprets these as red, but they are reported as `UNKNOWN` in
order to aim for 100% coverage of the `PASS`/`FAIL` scans.

The `XFAIL`, `FAIL`, `PASS` files are maintained in a git
repo. Changing policy requires modifying the policy in git, which is
logged and auditable.

Baseline Policy
---------------

Red Light Green Light can generate baseline XFAIL policy in cases
where you want to track regressions from an already imperfect test
run.

```shell
$ rlgl baseline --policy https://git.example.com/policy/dev.git my-test-report.html
```

This command writes XFAIL policy to the console — one matcher for
every `FAIL` in the report — which you can redirect into the `XFAIL`
file of your policy repo to excuse the current set of failures and
catch future regressions.


Policy in Detail
---------------

An `rlgl` policy consists of three separate files in a git repo:
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

A matchmaking expression may be followed by an expiration date, a time
after which the matchmaker no longer applies.

    # Whitelist this failure until April 1, 2019 and 9am
    { "result": "FAIL", "id": "CVE-2014-4043" } 2019-04-01 9:00

The date expiration time can be in any of the following formats:
RFC822 (RFC1123, RFC2822, RFC5322), asctime, RFC850 (RFC1036), ISO8601
(1988, 2000, 2004, except for no-year format), W3CDTF (subset of ISO
8601), RFC3339.  Examples of these include:

* `Thu, 23 Jul 2013 19:42:23 GMT` (RFC1123),
* `Thu Jul 23 19:42:23 2013` (asctime),
* `Thursday, 23-Jul-13 19:42:23 GMT` (RFC1036),
* `2013-07-23T19:42:23Z` (RFC3339),
* `20130723T194223Z` (ISO8601:2004), etc.

Red Light Green Light will also do it's best to interpret variations
of said standards, as in the example above (`2019-04-01 9:00`).

If a date is provided but no time, then it is interpreted as just after
midnight at the start of the day.

JSON matchmaking expressions cannot span more than one line of text.
This is required in order to attribute policy changes to individuals
via `git blame`.  These changelogs are available through the `rlgl`
reports generated at evaluation time.

Report Parsers
---------------

Currently supported report parsers include:

* [Anchore](https://anchore.com) container vulnerability json reports
* [AquaSec](https://github.com/aquasecurity/microscanner) container microscanner reports
* [Clair](https://github.com/coreos/clair) container scanner json reports
* [DejaGnu](https://www.gnu.org/software/dejagnu/) testing framework
* [JUnit](https://junit.org/junit5/) XML results report
* [OpenSCAP](https://www.open-scap.org/) OVAL scan reports
* [OpenSCAP](https://www.open-scap.org/) XCCDF scan reports
* [Popeye](https://popeyecli.io/) Popeye k8s sanitizer HTML reports
* [Tripwire](https://www.tripwire.com/) host scan reports in PDF format
* Comma separated values (CSV) for generic policy enforcement on arbitrary metrics (file size, performance results, etc).

Note that for the CSV parser, the first line of the CSV file defines
the field strings used in the resulting JSON results objects.  For
example, this CSV file...

    filename, filesize
    a.out, 1234567
    b.out, 87908

..produces the following JSON results objects...

    { "filename": "a.out", "filesize": "1234567" }
    { "filename": "b.out", "filesize": "87908" }

..for you to write policy against...

    { "filesize": "0..1000000" }

Report recognition is driven by the small shell scripts in `recog.d/`,
and report parsing by the Common Lisp code in `parsers/`.  Adding a new
report type means adding a recognizer script and a parser.

Building
--------

`rlgl` is written in [Common Lisp](https://lisp-lang.org/) and built
with [SBCL](https://www.sbcl.org/) and
[ocicl](https://github.com/ocicl/ocicl) for dependency management.

```shell
$ ocicl install      # fetch dependencies
$ make rlgl          # build the ./rlgl binary
$ make check         # run the test suite
```

The build produces a standalone `rlgl` executable.  At runtime `rlgl`
needs the `recog.d/` report recognizers; it locates them via (in
order) the `--root` option, the `RLGL_ROOT` environment variable, the
directory of the running executable, or the current working directory.

Configuration
-------------

`rlgl` is configured entirely through optional environment variables:

| Environment Variable | Description                                                              |
|----------------------|--------------------------------------------------------------------------|
| `RLGL_ROOT`          | Installation directory containing `recog.d/` report recognizers          |
| `RLGL_POLICY_DIR`    | Directory where policy git repos are cloned (default: `$XDG_CACHE_HOME/rlgl/policies`) |

Author and License
-------------------

Red Light Green Light was written by [Anthony
Green](https://github.com/atgreen), and is distributed under the terms
of the GNU Affero General Public License, Version 3.  See
[COPYING](https://raw.githubusercontent.com/atgreen/red-light-green-light/master/COPYING)
for details.
