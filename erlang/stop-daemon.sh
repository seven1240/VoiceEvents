#!/bin/sh
cd `dirname $0`
erl_call -a 'voice_events stop' -name 'voiceEvents' -c ClueCon
erl_call -a 'init stop' -name 'voiceEvents' -c ClueCon
