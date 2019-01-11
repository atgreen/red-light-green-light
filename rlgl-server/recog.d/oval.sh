#!/bin/sh

grep -q "<title>OVAL Results</title>" $1
if [ $? -eq 0 ]; then
    echo "oscap-oval";
fi
