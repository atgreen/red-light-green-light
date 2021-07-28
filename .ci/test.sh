#!/bin/sh

set -e

cd test
curl -s http://${1}/cli/rlgl-linux-amd64.tgz | \
    tar --strip-components=2 -xvzf - ./rlgl/rlgl

./rlgl login --key=AAAAAAA-BBBBBBB-CCCCCCC-DDDDDDD http://${1}
ID=$(./rlgl start ${1})

OUT=$(./rlgl e --id=$ID --policy=https://github.com/atgreen/test-policy ./report.html)
echo $OUT
REPORT=$(echo $OUT | awk '{ print $2 }' | cut -f2 -d=)
./rlgl verify $REPORT | RLGL_CLIENT_PUBKEY=~/.config/rlgl/public_key.pem sh
