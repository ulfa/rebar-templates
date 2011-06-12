#!/bin/sh

cat $PWD/test/test.spec.in | sed -e "s,@PATH@,$PWD," > $PWD/test/test.spec

