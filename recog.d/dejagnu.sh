#!/bin/sh

(grep -q "Test [Rr]un [Bb]y " $1 \
  && grep -q "Schedule of variations" $1 \
  && echo "dejagnu") \
 || true;
