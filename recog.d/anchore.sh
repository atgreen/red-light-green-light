#!/bin/sh

(head -n 3 $1 | grep -q "imageDigest" && echo "anchore") || true;


