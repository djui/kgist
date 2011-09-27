-module(kgist).

%%% Exports ====================================================================
-export([ start/0
        , stop/0
        ]).

%%% Imports ====================================================================
-import(tulib_application, [ ensure_started/1
                           ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
start() ->
  %% Logger
  %% Needs to be configured first
  %% application:start(riak_err),
  %% DB
  ensure_started(mnesia),
  kgist_db:ensure_initialized(),
  {ok, DBDir} = application:get_env(mnesia, dir),
  kgist_db:backup(filename:join([DBDir, "kgist.BUP"])),
  %% Web Server
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(mochiweb),
  application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
  ensure_started(webmachine),
  %% Kgist
  application:start(kgist).

stop() ->
  Res = application:stop(kgist),
  application:stop(webmachine),
  application:stop(mochiweb),
  application:stop(mnesia),
  %% application:stop(riak_err),
  application:stop(crypto),
  application:stop(inets),
  Res.
