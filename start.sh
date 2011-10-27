#!/bin/sh

ERL=erl

cd $(dirname $0)
exec $ERL -pa $PWD/ebin $PWD/deps/*/ebin \
          -sname kgist                   \
          -config sys                    \
          -s kgist                       \
          -heart                         \
        # -boot start_sasl               \
        # -init_debug
