-module(pygments).
-behaviour(gen_server).

%%% Exports ====================================================================
-export([ start_link/0
        , pygmentize/2
        ]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%% Records ====================================================================
-record(state, { view_dir
               , views
               }).

%%% Defines ====================================================================
-define(SERVER, ?MODULE).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
pygmentize(CodeText, Language) ->
  gen_server:call(?SERVER, {pygmentize, CodeText, Language}).

%%% Callbacks ------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  DefaultLanguage = application:get_env(default_language),
  Cmd   = os:find_executable("pygmentize"),
  Flags = [ {"-l", DefaultLanguage     }
          , {"-f", "html"              }
          , {"-P", "encoding=utf-8"    }
          , {"-P", "linenos=inline"    }
          , {"-P", "linenospecial=2"   }
          , {"-P", "lineanchors=linum" }
          , {"-P", "anchorlinenos=true"}
          ],
  {ok, #state{ cmd   = Cmd
             , flags = orddict:from_list(Flags)
             }}.

handle_call({pygmentize, CodeText, Language}, _From,
            State=#state{cmd=Cmd, flags=Flags}) ->
  CodeHighlighted = do_pygmentize(Cmd, Flags, Language, CodeText),
  {reply, CodeHighlighted, State};
handle_call(_, _From, State) ->
  {reply, undefined, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  true = erlang:port_close(Port),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internals ------------------------------------------------------------------
do_pygmentize(Cmd, Flags0, Language, CodeText) ->
  Flags = orddict:store("-l", Language, Flags0),
  Args  = orddict:fold(fun({K,V}, Acc) -> Acc++[K,V] end, [], Flags),
  try
    Port = erlang:open_port({spawn_executable, Cmd}, [ {args, Args}
                                                     , std_inout
                                                     , exit_status
                                                     ]),
    do_pygmentize_loop(Port, []),
  catch
    _C:R ->
      {error, R}
  end.

do_pygmentize_loop(Port, Data0) ->
  receive
    {Port, {data, Data}} ->
      do_pygmentize_loop(Port, Data0 ++ Data);
    {Port, {exit_status, Status}} ->
      {ok, Status};
    {'EXIT', Port, Reason} ->
      {error, Reason}
  end.



%% REMOVE
os_cmd(Cmd) ->
  try
    Port = erlang:open_port({spawn_executable, Cmd}, [exit_status]),
    Res = os_cmd_loop(Port, []),
    true = erlang:close_port(Port)
  catch
    _C:R ->
      {error, R}
  end.

os_cmd_loop(Port, Data0) ->
  receive
    {Port, {data, Data}} ->
      os_cmd_loop(Port, Data0 ++ Data);
    {Port, {exit_status, Status}} ->
      {ok, Status};
    {'EXIT', Port, Reason} ->
      {error, Reason}
  end.
