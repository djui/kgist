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
  Cmd   = os:find_executable("pygmentize"),
  Flags = "",
  {ok, #state{ cmd   = Cmd
             , flags = Flags
             }}.

handle_call({pygmentize, CodeText, Language}, _From, State=#state{cmd=Cmd, flags=Flags}) ->
  CodeHighlighted = os_cmd(Cmd ++ " " ++ string:join(" ", Flags) ++ " " ++ Language ++ " - " ++ CodeText),
  {reply, CodeHighlighted, State};
handle_call(_, _From, State) ->
  {reply, undefined, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internals ------------------------------------------------------------------
os_cmd(Cmd) ->
  try
    Port = erlang:open_port({spawn_command, Cmd}, [exit_status]),
    os_cmd_loop(Port, [])
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
