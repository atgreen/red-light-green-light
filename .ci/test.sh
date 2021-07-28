#!/bin/sh

set -x

cd test
curl -s http://${1}/cli/rlgl-linux-amd64.tgz | \
    tar --strip-components=2 -xvzf - ./rlgl/rlgl

./rlgl login --key=AAAAAAA-BBBBBBB-CCCCCCC-DDDDDDD http://${1}
ID=$(./rlgl start ${1})

OUT=$(./rlgl e --id=$ID --policy=https://github.com/atgreen/test-policy ./report.html)
echo $OUT
echo $OUT | awk '{ print $2 }'
