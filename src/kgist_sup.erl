-module(kgist_sup).
-behaviour(supervisor).

%%% Exports ====================================================================
-export([ start_link/0
        , init/1
        ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").

%%% Code =======================================================================
%%% Callbacks ------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Host}     = application:get_env(webmachine, host),
  {ok, Port}     = application:get_env(webmachine, port),
  {ok, LogDir}   = application:get_env(webmachine, log_dir),
  {ok, Dispatch} = application:get_env(webmachine, dispatch),
  WebConfig = [ {ip,       Host}
              , {port,     Port}
              , {log_dir,  LogDir}
              , {dispatch, Dispatch}
              ],
  Webmachine = { webmachine_mochiweb
               , {webmachine_mochiweb, start, [WebConfig]}
               , permanent, 5000, worker, dynamic
               },
  {ok, ViewDir} = application:get_env(view_dir),
  MustacheConfig = [ {view_dir, ViewDir}
                   , {layout_view, layout}
                   ],
  MustacheServer = { kgist_view
                   , {kgist_view, start_link, [MustacheConfig]}
                   , permanent, 5000, worker, [kgist_view]
                   },
  PygmentsServer = { pygments
                   , {pygments, start_link, []}
                   , permanent, 5000, worker, [pygments]
                   },
  SupTree = {{one_for_one, 10, 10}, [ Webmachine
                                    , MustacheServer
                                    , PygmentsServer
                                    ]},
  {ok, SupTree}.
