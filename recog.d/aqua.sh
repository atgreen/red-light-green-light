#!/bin/sh

grep -q "aqua_logo_" $1
if [ $? -eq 0 ]; then
    echo "aqua";
fi
