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
-record(state, { cmd
               , flags
               }).

%%% Defines ====================================================================
-define(SERVER, ?MODULE).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
pygmentize(CodeText, Language) ->
  gen_server:call(?SERVER, {pygmentize, Language, CodeText}).

%%% Callbacks ------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  DefaultLanguage = application:get_env(default_language),
  Cmd   = os:find_executable("pygmentize"),
  Flags = [ {language,    [ "-l", DefaultLanguage     ]}
          , {out_format,  [ "-f", "html"              ]}
          , {encoding,    [ "-P", "encoding=utf-8"    ]}
          , {linenumbers, [ "-P", "linenos=inline"
                          , "-P", "linenospecial=2"
                          , "-P", "lineanchors=linum"
                          , "-P", "anchorlinenos=true"]}
          ],
  {ok, #state{ cmd   = Cmd
             , flags = orddict:from_list(Flags)
             }}.

handle_call({pygmentize, Language, CodeText}, _From,
            State=#state{cmd=Cmd, flags=Flags0}) ->
  Flags = orddict:store(language, ["-l", Language], Flags0),
  Args  = orddict:fold(fun({_K,V}, Acc) -> V++Acc end, [], Flags),
  {ok, 0, CodeHighlighted} = do_pygmentize(Cmd, Args, CodeText),
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
do_pygmentize(Cmd, Args, Payload) ->
  Conf   = [std_inout, exit_status, {args, Args}],
  try
    Port = erlang:open_port({spawn_executable, Cmd}, Conf),
    true = erlang:port_command(Port, Payload),
    Res0 = do_pygmentize_loop(Port, []),
    true = erlang:port_close(Port),
    Res0
  catch _:R -> {error, R}
  end.

do_pygmentize_loop(Port, Data) ->
  receive
    {Port, {data, NewData}}       -> do_pygmentize_loop(Port, Data ++ NewData);
    {Port, {exit_status, Status}} -> {ok, Status, Data};
    {'EXIT', Port, Reason}        -> {error, Reason}
  end.
