-module(pygments).
-behaviour(gen_server).

%%% Exports ====================================================================
-export([ start_link/0
        , pygmentize/2
        , lang_alias/1
        , lang_mime/1
        , lang_ext/1
        , lang_exists/1
        ]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%% Imports ====================================================================
-import(tulib_application, [priv_file/1]).

%%% Records ====================================================================
-record(state, { cmd
               , flags     = []
               , languages = []
               }).

%%% Defines ====================================================================
-define(SERVER, ?MODULE).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
pygmentize(Language, CodeText) ->
  gen_server:call(?SERVER, {pygmentize, Language, CodeText}).

lang_alias(Language) ->
  gen_server:call(?SERVER, {lang_alias, Language}).

lang_ext(Language) ->
  gen_server:call(?SERVER, {lang_ext, Language}).

lang_mime(Language) ->
  gen_server:call(?SERVER, {lang_mime, Language}).

lang_exists(Language) ->
  gen_server:call(?SERVER, {lang_exists, Language}).

%%% Callbacks ------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, DefaultLanguage} = application:get_env(default_language),
  LanguagesPath         = priv_file("languages.eterm"),
  {ok, Languages}       = file:consult(LanguagesPath),
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
  {ok, #state{ cmd       = Cmd
             , flags     = orddict:from_list(Flags)
             , languages = Languages
             }}.

handle_call({pygmentize, Language, CodeText}, _From,
            State=#state{cmd=Cmd0, flags=Flags0, languages=Languages}) ->
  Lexer   = atom_to_list(lang(alias, Language, Languages)),
  Flags   = orddict:store(language, arg("-l", Lexer), Flags0),
  Args    = orddict:fold(fun(_Key, Value, Acc) -> Value++Acc end, [], Flags),
  Cmd     = string:join([Cmd0|Args], " "),
  {ok, S} = stdinout:start_link(Cmd),
  Res     = stdinout:send(S, CodeText),
  true    = stdinout:shutdown(S),
  HLCode  = lists:flatmap(fun erlang:binary_to_list/1, Res),
  {reply, HLCode, State};
handle_call({lang_alias, Language}, _From, State=#state{languages=Languages}) ->
  Alias = atom_to_list(lang(alias, Language, Languages)),
  {reply, Alias, State};
handle_call({lang_ext, Language}, _From, State=#state{languages=Languages}) ->
  Ext = lang(ext, Language, Languages),
  {reply, Ext, State};
handle_call({lang_mime, Language}, _From, State=#state{languages=Languages}) ->
  Mime = lang(mime, Language, Languages),
  {reply, Mime, State};
handle_call({lang_exists, Language}, _From, State=#state{languages=Languages}) ->
  Exists = proplists:is_defined(Language, Languages),
  {reply, Exists, State};
handle_call(_, _From, State) ->
  {reply, undefined, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', _Pid, shutdown}, State) ->
  {noreply, State};
handle_info(Info, State) ->
  error_logger:format("Got unknown info: ~p~n", [Info]),
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

lang(Key, Language, Languages) ->
  L      = proplists:get_value(Language, Languages, []),
  Values = proplists:get_value(Key, L, [undefined]),
  hd(Values).
