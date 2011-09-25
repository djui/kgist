-module(kgist_sup).
-behaviour(supervisor).

%%% Exports ====================================================================
-export([ start_link/0
        , init/1
        ]).

%%% Imports ====================================================================
-import(tulib_application, [get_env/2]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").

%%% Code =======================================================================
%%% Callbacks ------------------------------------------------------------------
start_link() ->
  WebConfig = [ {ip,       get_env(host, ?DEFAULT_HOST)}
              , {port,     get_env(port, ?DEFAULT_PORT)}
              , {log_dir,  get_env(log_dir, ?DEFAULT_LOG_DIR)}
              , {dispatch, dispatch()}
              ],
  Config = [ {webconfig, WebConfig}
           ],
  supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

init(Config) ->
  WebConfig = proplists:get_value(webconfig, Config),
  Webmachine = { webmachine_mochiweb
               , {webmachine_mochiweb, start, [WebConfig]}
               , permanent, 5000, worker, dynamic
               },
  SupTree = {{one_for_one, 10, 10}, [Webmachine]},
  {ok, SupTree}.

%%% Internals ------------------------------------------------------------------
dispatch() ->
  DispatchConf   = filename:join([code:priv_dir(kgist), "dispatch.conf"]),
  {ok, Dispatch} = file:consult(DispatchConf),
  Dispatch.
