#!/bin/sh

cat $PWD/data/test.spec.in | sed -e "s,@PATH@,$PWD," > $PWD/test/test.spec

