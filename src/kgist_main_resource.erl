-module(kgist_main_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ allowed_methods/2
        , content_types_provided/2
        , to_html/2
        , to_text/2
        ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%% Records ====================================================================
-record(ctx, { action
             }).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
init([]) ->
  init([overview]);
init([Action]) ->
  {ok, #ctx{action=Action}}.
    
%%% Callbacks ------------------------------------------------------------------
allowed_methods(ReqData, Ctx) ->
  {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  {[ {"text/plain", to_text}
   , {"text/html", to_html}
   ], ReqData, Ctx}.

to_text(ReqData, Ctx) ->
  Text = "TEXT",
  {Text, ReqData, Ctx}.

to_html(ReqData, Ctx=#ctx{action=overview}) ->
  HBody = "OVERVIEW",
  {HBody, ReqData, Ctx};

to_html(ReqData, Ctx=#ctx{action=new}) ->
  HBody = "NEW",
  {HBody, ReqData, Ctx}.

%%% Internals ------------------------------------------------------------------
