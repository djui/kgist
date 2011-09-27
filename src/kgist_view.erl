-module(kgist_view).
-behaviour(gen_server).

%%% Exports ====================================================================
-export([ start_link/0
        , reload/0
        , render/2
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
render(ViewId, ViewCtx) ->
  gen_server:call(?SERVER, {render, ViewId, ViewCtx}).

reload() ->
  gen_server:cast(?SERVER, reload).

%%% Callbacks ------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, ViewDir} = application:get_env(view_dir),
  Views         = reload_views(ViewDir),
  {ok, #state{ view_dir = ViewDir
             , views    = Views
             }}.

handle_call({render, ViewId, ViewCtx}, _From, State=#state{views=Views}) ->
  View    = dict:fetch(ViewId, Views),
  Content = do_render(View, ViewCtx),
  {reply, Content, State};
handle_call(_, _From, State) ->
  {reply, undefined, State}.

handle_cast(reload, State0=#state{view_dir=ViewDir}) ->
  Views = reload_views(ViewDir),
  State = State0#state{views=Views},
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internals ------------------------------------------------------------------
do_render(View, ViewCtx) ->
  mustache:render(undefined, View, ViewCtx).

reload_views(ViewDir) ->
  Load = fun(ViewFilename, Acc) ->
             ViewName   = list_to_atom(filename:basename(ViewFilename, ".html")),
             {ok, File} = file:read_file(ViewFilename),
             ViewFile   = binary_to_list(escape_parenthesis(File)),
             View       = mustache:compile(ViewFile),
             dict:store(ViewName, View, Acc)
         end,
  filelib:fold_files(ViewDir, ".*\.html$", false, Load, dict:new()).

escape_parenthesis(B) when is_binary(B) ->
  binary:replace(B, <<"\"">>, <<"\\\"">>, [global]);
escape_parenthesis(S) ->
  re:replace(S, "\"", "\\\\\"", [{return, list}, global]).
