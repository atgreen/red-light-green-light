rlgl - Red Light Green Light
===============================

Problem Statement
----------------

Modern DevOps pipelines require Quality Gating mechanisms in order to
fully automate the promotion of software artifacts from dev all the
way through to prod.  These Quality Gates are responsible for
evaluating deployable artifacts (config files, software builds,
container images, VM images, etc) as they go through a CI/CD pipeline,
to determine if they meet some minimum acceptable quality bar.

There are many tools available to evaluate the quality of a deployable
artefact, including container image scanners, unit test harnesses,
config file linters, etc.  The problem is that each one of the tools
generates custom output reports.  Some even provide custom mechnanisms
to control the evaluation process (eg. which test failures to ignore).

The idea behind Red Light Green Light is that we decouple of the test
evaluation policies from the underlying testing tools, in a way that
they are:

 - centrally managed
 - version controlled
 - auditable
 - customizeable
 - protected with authentication/authorization mechanisms

Here are the basic concepts:

- Each deployable artefact is given a Player ID.  The Player ID is
  what flows down the pipeline along with the various build/deploy
  artefacts.  They would be attached as artefact metadata.

```
$ ID=$(rlgl start)
```

- As the pipeline proceeds, test results are generated (scans, unit
  tests, etc).  For each test report generated, rlgl evaluates the
  report against the stated policy, resulting in a Red Light, meaning
  stop the pipeline, or Green Light, meaning proceed with the
  pipeline.

```shell
$ rlgl test --policy=dev $ID my-test-report.html
green
```

```shell
$ rlgl test --policy=global-prod $ID oval-scan.xml
red
```
   
```shell
$ rlgl test --policy=my-proj $ID gcc.log
green
```

That's it!

The client side is very easy.   


The server side, where policy is evaluated, is where the magic is.

The first step is to identify the type of report we're evaluating and
convert it into a canonical form.  The canonical form is defined
simply as this: a json object.  That's it.  No special schema is
defined.

Policy is also defined in plain text divided into three files: XFAIL,
FAIL, and PASS.  Each of these files contains a list of json matching
expressions to match again the canonical test results.  They are
evaluated this order: XFAIL, FAIL, PASS.

XFAIL contains matchers for test results we are expecting to fail and
allowing to pass anyway.  These are your exceptions.  Any matching
json objects are removed from the test results before processing with
FAIL.

FAIL contains matchers for tests results that are definitely failures.
The are removed from the test results before processing with PASS.

PASS contains matchers for known test passes.  These are removed from
the test results.

Any remaining entries in the test results are recorded as UNKNOWN.
Rlgl interprets these as failures, but they are reported as UNKNOWN in
order aim for 100% coverage of the PASS/FAIL scans.

The XFAIL, FAIL, PASS files are maintained in a git repo.  The git
repo (and credentials) are identified by the policy ID.  Changing
policy requires modifying the policy in git, which is logged and
auditable.


Managing Policy
================

This is a 'dev' policy:

    policy dev {
      url: https://github.com/atgreen/my-dev-rlgl-policy.git
      credetials: admin-creds
    }

The repo identied above contain XFAIL, FAIL and PASS files.

Policies can be composed of multiple policies by merging the contents of XFAIL, FAIL and PASS.

    policy dev {
      policy: dev
      policy: special-dev
    }

Policies can have expiration dates:

    policy dev {
      url: https://github.com/atgreen/my-dev-rlgl-policy.git
      credetials: admin-creds
      expires: 2019-02-01
    }

