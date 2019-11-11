#!/bin/sh

head $1 | grep -q "Test run by "
if [ $? -eq 0 ]; then
    echo "dejagnu";
fi
