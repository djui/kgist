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
  {ok, Port}     = application:get_env(misultin, port),
  {ok, LogDir}   = application:get_env(kgist, log_dir),
  {ok, ViewDir}  = application:get_env(kgist, view_dir),
  
  MustacheConfig = [ {view_dir,    ViewDir}
                   , {layout_view, layout }
                   ],
  MustacheServer = { kgist_view
                   , {kgist_view, start_link, [MustacheConfig]}
                   , permanent, 30000, worker, [kgist_view]
                   },
  PygmentsServer = { pygments
                   , {pygments, start_link, []}
                   , permanent, 30000, worker, [pygments]
                   },
  LoggerConfig   = [ {log_dir,  LogDir} ],
  LoggerServer   = { kgist_logger
                   , {kgist_logger, start_link, [LoggerConfig]}
                   , permanent, 30000, worker, [kgist_logger]
                   },
  MisultinConfig = [ {port, Port}
                   , {loop,       fun kgist_server:handle_http/1}
                   , {access_log, fun kgist_logger:access_log/1}
                   ],
  MisultinServer = { misultin
                   , {misultin, start_link, [MisultinConfig]}
                   , permanent, infinity, supervisor, [misultin]
                   },
  
  SupTree = {{one_for_one, 10, 10}, [ MustacheServer
                                    , PygmentsServer
                                    ,   LoggerServer
                                    , MisultinServer
                                    ]},
  {ok, SupTree}.
