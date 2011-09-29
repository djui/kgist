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
pygmentize(Language, CodeText) ->
  gen_server:call(?SERVER, {pygmentize, Language, CodeText}).

%%% Callbacks ------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  DefaultLanguage = application:get_env(default_language),
  Cmd   = os:find_executable("pygmentize"),
  Flags = [ {language,    arg("-l", DefaultLanguage)}
          , {out_format,  arg("-f", "html")}
          , {encoding,    param("encoding", "utf-8")}
          , {linenumbers, lists:concat([ param("linenos",       "inline")
                                       , param("linenospecial", "2")
                                       , param("lineanchors",   "linum")
                                       , param("anchorlinenos", "true")
                                       ])}
          ],
  {ok, #state{ cmd   = Cmd
             , flags = orddict:from_list(Flags)
             }}.

handle_call({pygmentize, Language, CodeText}, _From,
            State=#state{cmd=Cmd0, flags=Flags0}) ->
  Flags   = orddict:store(language, arg("-l", Language), Flags0),
  Args    = orddict:fold(fun(_Key, Value, Acc) -> Value++Acc end, [], Flags),
  Cmd     = string:join([Cmd0|Args], " "),
  {ok, S} = stdinout:start_link(Cmd),
  [Res]   = stdinout:send(S, CodeText),
  true    = stdinout:shutdown(S),
  HLCode  = binary_to_list(Res),
  {reply, HLCode, State};
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
arg(Flag, Value) ->
  [Flag, Value].

param(Key, Value) ->
  ["-P", Key ++ "=" ++ Value].
