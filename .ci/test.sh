#!/bin/sh

set -xe

cd test

# --- Install rlgl and rekor --------------------------------------------------
curl -s http://${1}/cli/rlgl-linux-amd64.tgz > rlgl-dl.tgz # | tar --strip-components=2 -xvzf - ./rlgl/rlgl
curl -Ls https://github.com/sigstore/rekor/releases/download/v0.11.0/rekor-cli-linux-amd64 > rekor-cli
chmod +x rekor-cli
./rekor-cli version
export PATH=$(pwd):$PATH

echo =========================================================================
echo Log in
echo =========================================================================
echo "rlgl login --key=AAAAAAA-BBBBBBB-CCCCCCC-DDDDDDD http://${1}"
rlgl login --key=AAAAAAA-BBBBBBB-CCCCCCC-DDDDDDD http://${1}

echo =========================================================================
echo Evaluate report
echo =========================================================================
echo "$ rlgl e --policy=https://github.com/atgreen/test-policy ./report.html"
OUT=$(rlgl e --policy=https://github.com/atgreen/test-policy ./report.html || true)
echo $OUT
REPORT=$(echo $OUT | awk '{ print $2 }' | cut -f2 -d=)
echo =========================================================================
echo Verify report
echo =========================================================================
echo "$ rlgl verify $REPORT | RLGL_CLIENT_PUBKEY=~/.config/rlgl/public_key.pem /bin/sh"
rlgl verify $REPORT | RLGL_CLIENT_PUBKEY=~/.config/rlgl/public_key.pem /bin/sh
