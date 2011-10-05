-module(kgist_main_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ allowed_methods/2
        , content_types_provided/2
        , to_html/2
        ]).


%%% Imports ====================================================================
-import(tulib_calendar, [ unix_timestamp/1 ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%% Records ====================================================================
-record(ctx, {}).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
init(_Config) ->
  {ok, #ctx{}}.
  
%%% Callbacks ------------------------------------------------------------------
allowed_methods(ReqData, Ctx) ->
  {['HEAD', 'GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  {[{"text/html", to_html}], ReqData, Ctx}.

to_html(ReqData, Ctx) ->
  Recents = kgist_view:to_list(kgist_db:recents()),
  ViewCtx = [ {gists, Recents}
            , {gist, "Foo"} %% TODO Set default values
            , {page_title, ""}
            ],
  HBody   = kgist_view:render(gist_new, ViewCtx),
  {HBody, ReqData, Ctx}.

%%% Internals ------------------------------------------------------------------
