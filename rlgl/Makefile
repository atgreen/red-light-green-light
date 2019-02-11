#! /usr/bin/make
#
# Makefile for Golang projects, v2
#
# Features:
# - uses github.com/Masterminds/glide to manage dependencies and uses GO15VENDOREXPERIMENT
# - runs ginkgo tests recursively, computes code coverage report
# - runs gofmt and go vet
# - prepares code coverage so travis-ci can upload it and produce badges for README.md
# - build for linux/amd64, linux/arm, darwin/amd64, windows/amd64
# - just 'make' builds for local OS/arch
# - produces .tgz/.zip build output
# - can bundle additional files into archive
# - sets a VERSION variable in the app
# - to include the build status and code coverage badge in CI use (replace NAME by what
#   you set $(NAME) to further down, and also replace magnum.travis-ci.com by travis-ci.org for
#   publicly accessible repos [sigh]):
#   [![Build Status](https://magnum.travis-ci.com/rightscale/NAME.svg?token=4Q13wQTY4zqXgU7Edw3B&branch=master)](https://magnum.travis-ci.com/rightscale/NAME
#   ![Code Coverage](https://s3.amazonaws.com/rs-code-coverage/NAME/cc_badge_master.svg)
#
# Top-level targets:
# default: compile the program, you can thus use make && ./NAME -options ...
# build: builds binaries for linux and darwin
# test: runs unit tests recursively and produces code coverage stats and shows them
# travis-test: just runs unit tests recursively
# clean: removes build stuff

# name of this app, used as basename for almost everything
NAME=rlgl

#=== below this line ideally remains unchanged, add new targets at the end  ===

# dependencies that are used by the build&test process, these need to be installed in the
# global Go env and not in the vendor sub-tree
DEPEND=golang.org/x/tools/cmd/cover github.com/onsi/ginkgo/ginkgo \
       github.com/onsi/gomega github.com/rlmcpherson/s3gof3r/gof3r \
       github.com/Masterminds/glide github.com/golang/lint/golint

TRAVIS_BRANCH?=dev
DATE=$(shell date '+%F %T')
TRAVIS_COMMIT?=$(shell git symbolic-ref HEAD | cut -d"/" -f 3)
GO15VENDOREXPERIMENT=1
export GO15VENDOREXPERIMENT

# produce a version string that is embedded into the binary that captures the branch, the date
# and the commit we're building. This works particularly well if you are using release branch
# names of the form "v1.2.3"
VERSION=$(NAME) $(TRAVIS_BRANCH) - $(DATE) - $(TRAVIS_COMMIT)
VFLAG=-X 'main.VERSION=$(VERSION)'

.PHONY: depend clean default

# the default target builds a binary in the top-level dir for whatever the local OS is
# it does not depend on 'depend' 'cause it's a pain to have that run every time you hit 'make'
# instead you get to 'make depend' manually once
default: $(NAME)
$(NAME): $(shell find . -name \*.go)
	go build -ldflags "$(VFLAG)" -o $(NAME) .

# the standard build produces a "local" executable, a linux tgz, and a darwin (macos) tgz
# uncomment and join the windows zip if you need it
build: $(NAME) build/$(NAME)-linux-amd64.tgz build/$(NAME)-darwin-amd64.tgz build/$(NAME)-linux-arm.tgz build/$(NAME)-windows-amd64.zip

# create a tgz with the binary and any artifacts that are necessary
# note the hack to allow for various GOOS & GOARCH combos, sigh
build/$(NAME)-%.tgz: *.go
	rm -rf build/$(NAME)
	mkdir -p build/$(NAME)
	tgt=$*; GOOS=$${tgt%-*} GOARCH=$${tgt#*-} go build -ldflags "$(VFLAG)" -o build/$(NAME)/$(NAME) .
	chmod +x build/$(NAME)/$(NAME)
	cp README.md build/$(NAME)/
	tar -zcf $@ -C build ./$(NAME)
	rm -r build/$(NAME)

build/$(NAME)-%.zip: *.go
	rm -rf build/$(NAME)
	mkdir -p build/$(NAME)
	tgt=$*; GOOS=$${tgt%-*} GOARCH=$${tgt#*-} go build -ldflags "$(VFLAG)" -o build/$(NAME)/$(NAME).exe .
	chmod +x build/$(NAME)/$(NAME).exe
	cp README.md build/$(NAME)/
	(cd build; zip -r `basename $@` $(NAME))
	rm -r build/$(NAME)

# Installing build dependencies. You will need to run this once manually when you clone the repo
depend:
	go get -v $(DEPEND)
	glide install

clean:
	rm -rf build .vendor/pkg rlgl *~

# run gofmt and complain if a file is out of compliance
# run go vet and similarly complain if there are issues
# run go lint and complain if there are issues
# TODO: go tool vet is currently broken with the vendorexperiement
lint:
	@if gofmt -l . | egrep -v ^vendor/ | grep .go; then \
	  echo "^- Repo contains improperly formatted go files; run gofmt -w *.go" && exit 1; \
	  else echo "All .go files formatted correctly"; fi
	#go tool vet -v -composites=false *.go
	#go tool vet -v -composites=false **/*.go
	for pkg in $$(go list ./... |grep -v /vendor/); do golint $$pkg; done

travis-test: cover

# running ginkgo twice, sadly, the problem is that -cover modifies the source code with the effect
# that if there are errors the output of gingko refers to incorrect line numbers
# tip: if you don't like colors use gingkgo -r -noColor
test: lint
	ginkgo -r -skipPackage vendor --randomizeAllSpecs --randomizeSuites --failOnPending

race: lint
	ginkgo -r -skipPackage vendor --randomizeAllSpecs --randomizeSuites --failOnPending --race


cover: lint
	ginkgo -r -skipPackage vendor --randomizeAllSpecs --randomizeSuites --failOnPending -cover
	@for d in `echo */*suite_test.go`; do \
	  dir=`dirname $$d`; \
	  (cd $$dir; go test -ginkgo.randomizeAllSpecs -ginkgo.failOnPending -cover -coverprofile $$dir_x.coverprofile -coverpkg $$(go list ./...|egrep -v vendor)); \
	done
	@rm -f _total
	@for f in `find . -name \*.coverprofile`; do tail -n +2 $$f >>_total; done
	@echo 'mode: atomic' >total.coverprofile
	@awk -f merge-profiles.awk <_total >>total.coverprofile
	COVERAGE=$$(go tool cover -func=total.coverprofile | grep "^total:" | grep -o "[0-9\.]*");\
	  echo "*** Code Coverage is $$COVERAGE% ***"
