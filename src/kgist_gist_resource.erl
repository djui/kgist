-module(kgist_gist_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ allowed_methods/2
        , content_types_provided/2
        , content_types_accepted/2
        , create_path/2
        , delete_resource/2
        , delete_completed/2
        , encodings_provided/2
        , expires/2
        , generate_etag/2
        , is_gist/1
        , last_modified/2
        , post_is_create/2
        , resource_exists/2
        , to_html/2
        , to_text/2
        ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%% Records ====================================================================
-record(ctx, { action
             , resource
             }).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
init(Config) ->
  Action = proplists:get_value(action, Config, show),
  {ok, #ctx{action=Action}}.
    
%%% Callbacks ------------------------------------------------------------------
allowed_methods(ReqData, Ctx) ->
  {['HEAD', 'GET', 'PUT', 'POST', 'DELETE'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  {[ {"text/plain", to_text}
   , {"text/html",  to_html}
   ], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  {[ {"text/plain", from_text}
   , {"text/html", from_html}
   ], ReqData, Ctx}.

create_path(ReqData, Ctx) ->
  _Value = wrq:req_body(ReqData),
  {{error, xxx}, ReqData, Ctx}.

encodings_provided(ReqData, Ctx) ->
  {[ {"identity", fun(X) -> X end}
   , {"gzip",     fun(X) -> zlib:gzip(X) end}
   ], ReqData, Ctx}.

delete_resource(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

delete_completed(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

expires(ReqData, Ctx) ->
  {undefined, ReqData, Ctx}.

generate_etag(ReqData, Ctx) ->
  {undefined, ReqData, Ctx}.

last_modified(ReqData, Ctx) ->
  {undefined, ReqData, Ctx}.

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  Id = wrq:path_info(id, ReqData),
  {ok, GistId} = gist_id(Id),
  case kgist_db:get(GistId) of
    {ok, Gist} -> {true, ReqData, Ctx#ctx{resource=Gist}};
    {error, _} -> {false, ReqData, Ctx}
  end.

to_text(ReqData0, Ctx=#ctx{action=download, resource=Gist}) ->
  ReqData = wrq:set_resp_header("content-disposition",
                                "attachment; filename=" ++ Gist#gist.filename,
                                ReqData0),
  Text = Gist#gist.code,
  {Text, ReqData, Ctx};
to_text(ReqData, Ctx=#ctx{resource=Gist}) ->
  Text = Gist#gist.code,
  {Text, ReqData, Ctx}.

to_html(ReqData, Ctx=#ctx{action=raw}) ->
  to_text(ReqData, Ctx);
to_html(ReqData, Ctx=#ctx{action=download}) ->
  to_text(ReqData, Ctx);
to_html(ReqData, Ctx=#ctx{action=show, resource=Gist}) ->
  TplCtx = dict:from_list([ {id,      Gist#gist.id}
                          , {hl_code, Gist#gist.code_highlighted}
                          ]),
  HBody = kgist_view:render(index, TplCtx),
  {HBody, ReqData, Ctx}.

is_gist(ReqData) ->
  Id = wrq:path_info(id, ReqData),
  case gist_id(Id) of
    error   -> false;
    {ok, _} -> true
  end.

%%% Internals ------------------------------------------------------------------
gist_id(S) ->
  case string:to_integer(S) of
    {error, _} ->
      error;
    {GistId, []} ->
      case GistId >= 1000 andalso GistId =< 9999 of
        true  -> {ok, GistId};
        false -> error
      end;
    _ ->
      error
  end.
