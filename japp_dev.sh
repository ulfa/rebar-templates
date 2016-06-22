#!/bin/sh
mkdir -p log
erl -sname {{appid}} -setcookie nocookie -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s {{appid}} -s observer -config ../../janga/etc/app.config
