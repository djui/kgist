#!/bin/sh

cd $(dirname $0)
# erl -name kgist -pa $PWD/ebin $PWD/deps/*/ebin -s kgist
exec erl -name kgist                    \
         -pa $PWD/ebin $PWD/deps/*/ebin \
         -heart                         \
         -init_debug                    \
         -boot start_sasl               \
         -s kgist
