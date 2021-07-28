#!/bin/sh

set -ex

cd test

# --- Install rlgl and rekor --------------------------------------------------
curl -s http://${1}/cli/rlgl-linux-amd64.tgz | tar --strip-components=2 -xvzf - ./rlgl/rlgl
curl -s https://github.com/sigstore/rekor/releases/download/v0.3.0/rekor-cli-linux-amd64 > rekor-cli
chmod +x rekor-cli
./rekor-cli version

export PATH=$(pwd):$PATH
rekor-cli version

rlgl login --key=AAAAAAA-BBBBBBB-CCCCCCC-DDDDDDD http://${1}
ID=$(rlgl start ${1})

OUT=$(rlgl e --id=$ID --policy=https://github.com/atgreen/test-policy ./report.html || true)
echo $OUT
REPORT=$(echo $OUT | awk '{ print $2 }' | cut -f2 -d=)
rlgl verify $REPORT
rlgl verify $REPORT | RLGL_CLIENT_PUBKEY=~/.config/rlgl/public_key.pem sh
