#!/bin/sh
cd `dirname $0`

export RUN_ERL_LOG_GENERATIONS=100
export RUN_ERL_LOG_MAXSIZE=100000000
run_erl -daemon /home/app/shared/erlang_logs/telegraph/  /home/app/shared/erlang_logs/telegraph "exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -name voiceEvents@192.168.1.30 -setcookie ClueCon -s reloader -s voiceEvents"

