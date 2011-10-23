-module(kgist).

%%% Exports ====================================================================
-export([ start/0
        , stop/0
        ]).

%%% Imports ====================================================================
-import(tulib_application, [ ensure_started/1
                           , priv_file/2
                           ]).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
start() ->
  %% Logging
  LogDir = priv_file(kgist, filename:join(["logs", "server.log"])),
  ok = error_logger:logfile({open, LogDir}),
  %% Applications
  ensure_started(mnesia),
  kgist_db:ensure_initialized(),
  ensure_started(misultin),
  application:start(kgist).

stop() ->
  Res = application:stop(kgist),
  application:stop(misultin),
  application:stop(mnesia),
  error_logger:logfile(close),
  Res.
