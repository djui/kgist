-module(kgist).

%%% Exports ====================================================================
-export([ start/0
        , stop/0
        ]).

%%% Imports ====================================================================
-import(tulib_application, [ ensure_started/1 ]).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
start() ->
  ensure_started(mnesia),
  kgist_db:ensure_initialized(),
  ensure_started(misultin),
  application:start(kgist).

stop() ->
  Res = application:stop(kgist),
  application:stop(misultin),
  application:stop(mnesia),
  Res.
