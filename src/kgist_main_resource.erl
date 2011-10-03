-module(kgist_main_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ allowed_methods/2
        , content_types_provided/2
        , encodings_provided/2
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

encodings_provided(ReqData, Ctx) ->
  {[ {"identity", fun(X) -> X end}
   , {"gzip",     fun(X) -> zlib:gzip(X) end}
   ], ReqData, Ctx}.

to_html(ReqData, Ctx) ->
  Now      = unix_timestamp(erlang:now()),
  OneWeek  = 604800, %% 7*24*60*60
  LastWeek = Now - OneWeek,
  Recents  = kgist_db:get_since(LastWeek),
  ViewCtx  = [ {gists, Recents}
             , {gist, kgist_db:default()}
             , {page_title, ""}
             ],
  HBody    = kgist_view:render(gist_new, ViewCtx),
  {HBody, ReqData, Ctx}.
