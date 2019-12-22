#!/bin/sh

grep -q "OpenSCAP Evaluation Report</title>" $1
if [ $? -eq 0 ]; then
    echo "oscap-xccdf";
fi
