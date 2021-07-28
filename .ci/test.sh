#!/bin/sh

set -x

mkdir test
cd test
curl -s http://${1}/cli/rlgl-linux-amd64.tgz | \
    tar --strip-components=2 -xvzf - ./rlgl/rlgl

./rlgl --id=${RLGL_KEY} login ${1}
./rlgl start ${1}
pwd
