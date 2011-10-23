-module(kgist_view).
-behaviour(gen_server).

%%% Exports ====================================================================
-export([ start_link/1
        , reload/0
        , render/2
        , render_partial/2
        , to_list/1
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
               , layout_view
               , views
               }).

%%% Defines ====================================================================
-define(SERVER, ?MODULE).
-define(is_dict(D), is_record(D, dict, 8)). %% Size included as we can't include
                                            %% the record def from dict.erl

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
render(ViewId, ViewCtx) ->
  gen_server:call(?SERVER, {render, ViewId, ViewCtx}).

render_partial(ViewId, ViewCtx) ->
  gen_server:call(?SERVER, {render_partial, ViewId, ViewCtx}).

reload() ->
  gen_server:cast(?SERVER, reload).

to_list(L) -> lists:map(fun kgist_gist:to_dict/1, L).

%%% Callbacks ------------------------------------------------------------------
start_link(Config) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

init(Config) ->
  ViewDir    = proplists:get_value(view_dir, Config),
  LayoutView = proplists:get_value(layout_view, Config),
  Views      = reload_views(ViewDir),
  {ok, #state{ view_dir    = ViewDir
             , layout_view = LayoutView
             , views       = Views
             }}.

handle_call({render, ViewId, ViewCtx0}, _From,
            State=#state{views=Views, layout_view=LayoutViewId}) ->
  View        = dict:fetch(ViewId, Views),
  LayoutView  = dict:fetch(LayoutViewId, Views),
  ViewCtx     = ensure_dict(ViewCtx0),
  Body        = do_render(View, ViewCtx),
  {ok, Email} = application:get_env(kgist, admin_email),
  LayoutCtx1  = dict:store(admin_email, Email, ViewCtx),
  LayoutCtx2  = dict:store(body, Body, LayoutCtx1),
  Content     = do_render(LayoutView, LayoutCtx2),
  {reply, Content, State};
handle_call({render_partial, ViewId, ViewCtx0}, _From,
            State=#state{views=Views}) ->
  ViewCtx = ensure_dict(ViewCtx0),
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
  filelib:fold_files(ViewDir, ".*\.html$", false, Load, dict:new()). %%"

escape_parenthesis(B) when is_binary(B) ->
  binary:replace(B, <<"\"">>, <<"\\\"">>, [global]);
escape_parenthesis(S) ->
  re:replace(S, "\"", "\\\\\"", [{return, list}, global]).

ensure_dict(ViewCtx) when ?is_dict(ViewCtx) -> ViewCtx;
ensure_dict(ViewCtx)                        -> dict:from_list(ViewCtx).
