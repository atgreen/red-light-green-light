#!/bin/sh

head $1 | grep -q "Test [Rr]un [Bb]y "
if [ $? -eq 0 ]; then
    echo "dejagnu";
fi
