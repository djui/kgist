-module(kgist_static_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ allowed_methods/2
        , content_types_provided/2
        , encodings_provided/2
        , generate_etag/2
        , last_modified/2
        , provide_content/2
        , resource_exists/2
        ]).

%%% Includes ===================================================================
-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%% Records ====================================================================
-record(ctx, { root
             , response_body
             , metadata=[]
             }).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
init(Config) ->
  Root      = proplists:get_value(root, Config),
  {ok, App} = application:get_application(),
  PrivDir   = code:priv_dir(App),
  AbsRoot   = filename:absname(Root, PrivDir),
  {ok, #ctx{root=AbsRoot}}.

%%% Callbacks ------------------------------------------------------------------
allowed_methods(ReqData, Ctx) ->
  {['HEAD', 'GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  CT = webmachine_util:guess_mime(wrq:disp_path(ReqData)),
  {[{CT, provide_content}], ReqData, Ctx#ctx{metadata=[{ 'content-type'
                                                       , CT
                                                       }|Ctx#ctx.metadata]}}.

encodings_provided(ReqData, Ctx) ->
  {[ {"identity", fun(X) -> X end}
   , {"gzip",     fun(X) -> zlib:gzip(X) end}
   ], ReqData, Ctx}.

generate_etag(ReqData, Ctx) ->
  case maybe_fetch_object(Ctx, wrq:disp_path(ReqData)) of
    {true, BodyCtx} ->
      ETag = hash_body(BodyCtx#ctx.response_body),
      {ETag, ReqData, BodyCtx#ctx{metadata=[{etag,ETag}|BodyCtx#ctx.metadata]}};
    _ ->
      {undefined, ReqData, Ctx}
  end.

last_modified(ReqData, Ctx) ->
  {true, FullPath} = file_exists(Ctx, wrq:disp_path(ReqData)),
  LMod = filelib:last_modified(FullPath),
  {LMod, ReqData, Ctx#ctx{metadata=[{ 'last-modified'
                                    , httpd_util:rfc1123_date(LMod)
                                    }|Ctx#ctx.metadata]}}.

resource_exists(ReqData, Ctx) ->
  Path = wrq:disp_path(ReqData),
  case file_exists(Ctx, Path) of
    {true, _} -> {true,  ReqData, Ctx};
    false     -> {false, ReqData, Ctx}
  end.

%%% Internals ------------------------------------------------------------------
provide_content(ReqData, Ctx) ->
  case maybe_fetch_object(Ctx, wrq:disp_path(ReqData)) of
    {true, NewCtx} ->
      Body = NewCtx#ctx.response_body,
      {Body, ReqData, Ctx};
    {false, NewCtx} ->
      {error, ReqData, NewCtx}
  end.

maybe_fetch_object(Ctx, Path) ->
  %% if returns {true, NewCtx} then NewCtx has response_body
  case Ctx#ctx.response_body of
    undefined ->
      case file_exists(Ctx, Path) of
        {true, FullPath} ->
          {ok, Value} = file:read_file(FullPath),
          {true, Ctx#ctx{response_body=Value}};
        false ->
          {false, Ctx}
      end;
    _Body ->
      {true, Ctx}
  end.

file_exists(Ctx, Name) ->
  NamePath = file_path(Ctx, Name),
  case filelib:is_regular(NamePath) of
    true  -> {true, NamePath};
    false -> false
  end.

file_path(_Ctx,          "") -> undefined;
file_path(Ctx,    "/"++Name) -> file_path(Ctx, Name);
file_path(Ctx,      RelName) -> filename:join([Ctx#ctx.root, RelName]).

hash_body(Body) ->
  mochihex:to_hex(binary_to_list(crypto:sha(Body))).
