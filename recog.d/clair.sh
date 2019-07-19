#!/bin/sh

grep -q "unapproved" $1
if [ $? -eq 0 ]; then
    grep -q "vulnerabilites" $1;
    if [ $? -eq 0 ]; then
# possibly test with json parser here...
	echo "clair";
    fi
fi
