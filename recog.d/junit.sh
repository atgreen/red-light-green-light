#!/bin/sh

grep -q "xml" $1
if [ $? -eq 0 ]; then
    grep -q "testsuite" $1;
    if [ $? -eq 0 ]; then
	grep -q "testcase" $1;
	if [ $? -eq 0 ]; then
	    echo "junit";
	fi
    fi
fi
