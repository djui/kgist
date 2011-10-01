-module(kgist_main_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ allowed_methods/2
        , content_types_provided/2
        , to_html/2
        , to_text/2
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
  {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  {[ {"text/plain", to_text}
   , {"text/html",  to_html}
   ], ReqData, Ctx}.

to_text(ReqData, Ctx) ->
  Text = "TEXT",
  {Text, ReqData, Ctx}.

to_html(ReqData, Ctx) ->
  Now         = unix_timestamp(erlang:now()),
  OneWeek     = 604800, %% 7*24*60*60
  LastWeek    = Now - OneWeek,
  RecentGists = kgist_db:get_since(LastWeek),
  ViewCtx   = [{gists, RecentGists}, {gist, kgist_db:default()}],
  Body      = kgist_view:render(gist_new, ViewCtx),
  LayoutCtx = [{page_title, ""}, {body, Body}],
  HBody     = kgist_view:render(layout, LayoutCtx),
  {HBody, ReqData, Ctx}.

%%% Internals ------------------------------------------------------------------
