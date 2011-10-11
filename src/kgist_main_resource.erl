-module(kgist_main_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ allowed_methods/2
        , content_types_provided/2
        , to_html/2
        ]).

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
%% @callback webmachine
allowed_methods(ReqData, Ctx) ->
  {['HEAD', 'GET'], ReqData, Ctx}.

%% @callback webmachine
content_types_provided(ReqData, Ctx) ->
  {[{"text/html", to_html}], ReqData, Ctx}.

%% @callback content_types_provided
to_html(ReqData, Ctx) ->
  Recents    = kgist_view:to_list(kgist_db:recents()),
  Author     = wrq:get_cookie_value("author", ReqData),
  {ok, Lang} = application:get_env(default_language),
  ViewCtx    = [ {page_title,    ""     }
               , {gists,         Recents}
               , {gist_author,   Author }
               , {gist_language, Lang   }
               ],
  HBody      = kgist_view:render(gist_new, ViewCtx),
  {HBody, ReqData, Ctx}.

