#!/bin/sh

grep -q "<title>Popeye Sanitizer Report</title>" $1
if [ $? -eq 0 ]; then
    echo "popeye";
fi
