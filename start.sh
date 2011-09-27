#!/bin/sh

cd $(dirname $0)
exec erl -sname kgist                   \
         -pa $PWD/ebin $PWD/deps/*/ebin \
         -heart                         \
         -init_debug                    \
         -boot start_sasl               \
         -config dev                    \
         -s kgist
