#!/bin/sh
cd `dirname $0`
export HEART_COMMAND=`dirname $0`/start.sh

exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -sname voiceEvents@localhost -setcookie ClueCon -s voiceEvents -config error_logger -detatch -heart
