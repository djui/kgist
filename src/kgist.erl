-module(kgist).

%%% Exports ====================================================================
-export([ start/0
        , stop/0
        ]).

%%% Imports ====================================================================
-import(tulib_application, [ensure_started/1]).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
start() ->
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(mnesia),
  ensure_db_initialized(),
  ensure_started(mochiweb),
  application:set_env(webmachine, webmachine_logger_module, 
                      webmachine_logger),
  ensure_started(webmachine),
  application:start(kgist).

stop() ->
  Res = application:stop(kgist),
  application:stop(webmachine),
  application:stop(mochiweb),
  application:stop(mnesia),  
  application:stop(crypto),
  application:stop(inets),
  Res.

%%% Internals ------------------------------------------------------------------
ensure_db_initialized() ->
  io:format("~p~n", [mnesia:system_info()]).
