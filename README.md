`rlgl` - Red Light Green Light
===============================

**Red Light Green Light** is a policy management and enforcement tool
  designed to accelerate your CI/CD pipelines.


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
 - policies are locked within proprietery tools

Policy Driven Quality Gates
------------------------------

The main idea behind Red Light Green Light is to decouple the test
evaluation mechanisms and policies from the underlying testing tools,
in a way that they are:

 - centrally managed
 - version controlled
 - auditable
 - customizeable
 - protected with authentication/authorization mechanisms

Here are the basic concepts:

- Each deployable artifact is given a Player ID.  The Player ID is
  what flows down the pipeline along with the various build/deploy
  artifacts.  They would be attached as artefact metadata.

```
$ ID=$(rlgl start)
```

- As the pipeline proceeds, test results are generated (scans, unit
  tests, etc).  For each test report generated, `rlgl` evaluates the
  report against the stated policy, resulting in a **Red Light**,
  meaning stop the pipeline, or **Green Light**, meaning proceed with
  the pipeline.  It also produces a URL, which links to a report
  showing annotated evaluation results.  Annotations, include, for
  example, the git logs for policies defining exceptions resulting in
  green lights.

```shell
$ rlgl evaluate --policy=dev $ID my-test-report.html
green: http://rlgl-server.example.com/bc7db3f.html
```

```shell
$ rlgl evaluate --policy=global-prod $ID oval-scan.xml
red: http://rlgl-server.example.com/1cf5b3a.html
```
   
```shell
$ rlgl evaluate --policy=my-proj $ID gcc.log
green: http://rlgl-server.example.com/afc7db2.html
```

That's it!   The client side is very easy.   

The server side, where policy is evaluated, is where the magic is.

The first step is to identify the type of report we're evaluating and
convert it into a canonical form.  The canonical form is defined
simply as this: a json object.  That's it.  No special schema is
defined.

Policy is also defined in plain text divided into three files: `XFAIL`,
`FAIL`, and `PASS`.  Each of these files contains a list of json matching
expressions to match again the canonical test results.  They are
evaluated this order: `XFAIL`, `FAIL`, `PASS`.

`XFAIL` contains matchers for test results we are expecting to fail and
allowing to pass anyway.  These are your exceptions.  Any matching
json objects are removed from the test results before processing with
FAIL.

`FAIL` contains matchers for tests results that are definitely failures.
The are removed from the test results before processing with `PASS`.

`PASS` contains matchers for known test passes.  These are removed from
the test results.

Any remaining entries in the test results are recorded as UNKNOWN.
`rlgl` interprets these as failures, but they are reported as UNKNOWN in
order aim for 100% coverage of the `PASS`/FAIL scans.

The `XFAIL`, `FAIL`, `PASS` files are maintained in a git repo.  The git
repo (and credentials) are identified by the policy ID.  Changing
policy requires modifying the policy in git, which is logged and
auditable.

The server side also records which policies have received green
lights, and can report on those facts.


Policy in Detail
---------------

As mentioned above, a `rlgl` policy consists of three separate files:
`XFAIL`, `FAIL` and `PASS`. Each file contains JSON matchmaking
expressions as defined here:
https://github.com/chancancode/json_expressions.

For example, to mark a CVE failure as an exception, we add the
following to our `XFAIL` file:

    # Ignore this failure in our container images
    { id: "CVE-2014-4043" }

To ignore all CVEs with a score of less than 7 we add the following to
our `XFAIL` file:

    # Ignore everything but the most critical CVEs.
    { score: "0..6" }


Managing Policy
------------

This is a 'dev' policy:

    policy dev {
      url: https://github.com/atgreen/my-dev-rlgl-policy.git
      credentials: admin-creds
    }

The repo identied above contain `XFAIL`, `FAIL` and `PASS` files.

Policies can be composed of multiple policies by merging the contents
of `XFAIL`, `FAIL` and `PASS`.

    policy dev {
      policy: dev
      policy: special-dev
    }

Policies can have expiration dates:

    policy dev {
      url: https://github.com/atgreen/my-dev-rlgl-policy.git
      credentials: admin-creds
      expires: 2019-02-01
    }

