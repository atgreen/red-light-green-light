#!/bin/sh

grep -q "unapproved" $1
if [ $? -eq 0 ]; then
    grep -q "vulnerabilities" $1;
    if [ $? -eq 0 ]; then
	echo "clair";
    fi
fi
