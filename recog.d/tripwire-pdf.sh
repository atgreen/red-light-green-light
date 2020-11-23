#!/bin/sh

cp $1 /tmp/$1.copy

(file $1 | grep -q PDF \
     && pdfinfo $1 | grep -q "Title:          PolicyReport" \
     && echo "tripwire-pdf") || true
