#!/bin/sh

cd $(dirname $0)
exec erl -pa $PWD/ebin $PWD/deps/*/ebin \
         -sname kgist                   \
         -s kgist                       \
         -config dev                    \
#         -heart                         \
#         -init_debug                    \
#         -boot start_sasl
