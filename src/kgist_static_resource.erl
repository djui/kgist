-module(kgist_static_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ accept_content/2
        , allowed_methods/2
        , content_types_provided/2
        , content_types_accepted/2
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
init(ConfigProps) ->
  {root, Root} = proplists:lookup(root, ConfigProps),
  {ok, #ctx{root=Root}}.

%%% Callbacks ------------------------------------------------------------------
accept_content(ReqData, Ctx) ->
  Path = wrq:disp_path(ReqData),
  FP = file_path(Ctx, Path),
  ok = filelib:ensure_dir(FP),
  ReqData1 = case file_exists(Ctx, Path) of
               {true, _} ->
                 ReqData;
               _ ->
                 LOC = "http://" ++
                   wrq:get_req_header("host", ReqData) ++
                   "/fs/" ++ Path,
                 wrq:set_resp_header("Location", LOC, ReqData)
             end,
  Value = wrq:req_body(ReqData1),
  case file:write_file(FP, Value) of
    ok ->
      {true, wrq:set_resp_body(Value, ReqData1), Ctx};
    Err ->
      {{error, Err}, ReqData1, Ctx}
  end.    

allowed_methods(ReqData, Ctx) ->
  {['HEAD', 'GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  CT = webmachine_util:guess_mime(wrq:disp_path(ReqData)),
  {[{CT, provide_content}], ReqData,
   Ctx#ctx{metadata=[{'content-type', CT}|Ctx#ctx.metadata]}}.

content_types_accepted(ReqData, Ctx) ->
  CT = case wrq:get_req_header("content-type", ReqData) of
         undefined -> "application/octet-stream";
         X -> X
       end,
  {MT, _Params} = webmachine_util:media_type_to_detail(CT),
  {[{MT, accept_content}], ReqData,
   Ctx#ctx{metadata=[{'content-type', MT}|Ctx#ctx.metadata]}}.

generate_etag(ReqData, Ctx) ->
  case maybe_fetch_object(Ctx, wrq:disp_path(ReqData)) of
    {true, BodyCtx} ->
      ETag = hash_body(BodyCtx#ctx.response_body),
      {ETag, ReqData,
       BodyCtx#ctx{metadata=[{etag,ETag}| BodyCtx#ctx.metadata]}};
    _ ->
      {undefined, ReqData, Ctx}
  end.

last_modified(ReqData, Ctx) ->
  {true, FullPath} = file_exists(Ctx, wrq:disp_path(ReqData)),
  LMod = filelib:last_modified(FullPath),
  {LMod, ReqData, Ctx#ctx{metadata=[{'last-modified', httpd_util:rfc1123_date(LMod)}|
                                    Ctx#ctx.metadata]}}.

provide_content(ReqData, Ctx) ->
  case maybe_fetch_object(Ctx, wrq:disp_path(ReqData)) of
    {true, NewCtx} ->
      Body = NewCtx#ctx.response_body,
      {Body, ReqData, Ctx};
    {false, NewCtx} ->
      {error, ReqData, NewCtx}
  end.

resource_exists(ReqData, Ctx) ->
  Path = wrq:disp_path(ReqData),
  case file_exists(Ctx, Path) of
    {true, _} ->
      {true, ReqData, Ctx};
    _ ->
      case Path of
        "p" -> {true, ReqData, Ctx};
        _ -> {false, ReqData, Ctx}
      end
  end.

%%% Internals ------------------------------------------------------------------
file_path(_Ctx, []) ->
  false;
file_path(Ctx, Name) ->
  RelName = case hd(Name) of
              "/" -> tl(Name);
              _ -> Name
            end,
  filename:join([Ctx#ctx.root, RelName]).

file_exists(Ctx, Name) ->
  NamePath = file_path(Ctx, Name),
  case filelib:is_regular(NamePath) of
    true ->
      {true, NamePath};
    false ->
      false
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

hash_body(Body) ->
  mochihex:to_hex(binary_to_list(crypto:sha(Body))).
