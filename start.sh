#!/bin/sh

cd $(dirname $0)
exec erl -pa $PWD/ebin $PWD/deps/*/ebin \
         -sname kgist                   \
         -config dev                    \
         -s kgist                       \
       # -boot start_sasl               \
       # -heart                         \
       # -init_debug
