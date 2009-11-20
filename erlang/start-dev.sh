#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -name voiceEvents@192.168.1.14 -setcookie ClueCon -s reloader -s voiceEvents
