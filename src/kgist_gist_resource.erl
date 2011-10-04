-module(kgist_gist_resource).

%%% Exports ====================================================================
-export([ init/1 ]).
-export([ allowed_methods/2
        , allow_missing_post/2
        , content_types_provided/2
        , content_types_accepted/2
        , create_path/2
        , delete_resource/2
        , delete_completed/2
        , expires/2
        , from_form/2
        , generate_etag/2
        , is_gist/1
        , last_modified/2
        , post_is_create/2
        , resource_exists/2
        , to_html/2
        , to_text/2
        ]).

%%% Imports ====================================================================
-import(tulib_calendar, [unix_timestamp/1]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%% Records ====================================================================
-record(ctx, { action
             , resource
             , id
             }).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
init(Config) ->
  Action = proplists:get_value(action, Config),
  {ok, #ctx{action=Action}}.
  
%%% Callbacks ------------------------------------------------------------------
allowed_methods(ReqData, Ctx) ->
  {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], ReqData, Ctx}.

allow_missing_post(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  {[ {"text/plain", to_text}
   , {"text/html",  to_html}
   ], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  {[ {"application/x-www-form-urlencoded", from_form}
   ], ReqData, Ctx}.

create_path(ReqData, Ctx) ->
  Id = kgist_db:next_id(),
  {integer_to_list(Id), ReqData, Ctx#ctx{id=Id}}.

delete_resource(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

delete_completed(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

expires(ReqData, Ctx) ->
  {undefined, ReqData, Ctx}.

from_form(ReqData, Ctx) ->
  %% TODO Set author cookie
  Id      = Ctx#ctx.id,
  ReqBody = wrq:req_body(ReqData),
  Vals    = mochiweb_util:parse_qs(ReqBody),
  {ok, Gist} = spec(Vals),
  case kgist_db:put(Id, Gist) of
    ok ->
      ResBody  = "Gist " ++ integer_to_list(Id) ++ " added",
      Response = wrq:set_resp_body(ResBody, ReqData),
      {true, Response, Ctx};
    {error, Err} ->
      {Err, ReqData, Ctx}
  end.

generate_etag(ReqData, Ctx) ->
  {undefined, ReqData, Ctx}.

last_modified(ReqData, Ctx) ->
  {undefined, ReqData, Ctx}.

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  Id = wrq:path_info(id, ReqData),
  case gist_id(Id) of
    error        -> {false, ReqData, Ctx};
    {ok, GistId} ->
      case kgist_db:get(GistId) of
        {ok, Gist} -> {true, ReqData, Ctx#ctx{resource=Gist}};
        {error, _} -> {false, ReqData, Ctx}
      end
  end.
        
to_text(ReqData0, Ctx=#ctx{action=download, resource=Gist}) ->
  %% TODO Set content-type
  ReqData = attachment(Gist#gist.filename, ReqData0),
  Text = Gist#gist.code,
  {Text, ReqData, Ctx};
to_text(ReqData, Ctx=#ctx{resource=Gist}) ->
  %% TODO Set content-type
  Text = Gist#gist.code,
  {Text, ReqData, Ctx}.

to_html(ReqData, Ctx=#ctx{action=raw}) ->
  to_text(ReqData, Ctx);
to_html(ReqData, Ctx=#ctx{action=download}) ->
  to_text(ReqData, Ctx);
to_html(ReqData, Ctx=#ctx{resource=Gist}) ->
  ViewCtx = [ {id,         Gist#gist.id}
            , {hl_code,    Gist#gist.code_highlighted}
            , {page_title, Gist#gist.id}
            ],
  HBody   = kgist_view:render(gist_view, ViewCtx),
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
      end
  end.

attachment(Filename, ReqData) ->
  wrq:set_resp_header("content-disposition",
                      "attachment; filename=" ++ Filename,
                      ReqData).

spec(Vals) ->
  %% ETL Style
  %% Extract
  Author0  = proplists:get_value("author",      Vals),
  Code0    = proplists:get_value("code",        Vals),
  Desc0    = proplists:get_value("description", Vals),
  Expires0 = proplists:get_value("expires",     Vals),
  Irc0     = proplists:get_value("irc",         Vals),
  File0    = proplists:get_value("filename",    Vals),
  Lang0    = proplists:get_value("language",    Vals),
  %% Translate
  Author   = convert([non_empty, {max, 32}], Author0),
  Code     = convert([non_empty, {max, 1048576}], Code0),
  Expires  = convert([{in, ["1h","1d","1w","1m","1y"]}], Expires0),
  Desc     = convert([non_empty, {max, 1024}], Desc0),
  Lang     = convert([{fun pygments:lang_exists/1, "Text only"}], Lang0),
  DefFile  = "gistfile" ++ hd(pygments:lang_ext(Lang)),
  File     = convert([{non_empty, DefFile}], File0),
  Irc      = convert([{min, 2}, fun(["#"|_]) -> true; (_) -> false end], Irc0),
  HlCode   = pygments:pygmentize(Lang, Code),
  %% Load
  Gist = #gist{ creation_time    = unix_timestamp(now())
              , expires          = Expires
              , archived         = false
              , language         = Lang
              , author           = Author
              , filename         = File
              , description      = Desc
              , irc              = Irc
              , code             = Code
              , code_highlighted = HlCode
              },
  {ok, Gist}.

convert([], S)         -> S;
convert(_,  undefined) -> undefined;
convert([non_empty|Rules], S) ->
  convert([{non_empty, undefined}|Rules], S);
convert([{non_empty, Def}|Rules], "") ->
  convert(Rules, Def);
convert([{min, Len}|Rules], S) ->
  convert([fun(E) -> length(E) >= Len end|Rules], S);
convert([{max, Len}|Rules], S) ->
  convert(Rules, string:substr(S, 1, Len));
convert([{in, L, Def}|Rules], S) ->
  convert([{fun(E) -> lists:member(E, L) end, Def}|Rules], S);
convert([Guard|Rules], S) when is_function(Guard) ->
  convert([{Guard, undefined}|Rules], S);
convert([{Guard, Def}|Rules], S) when is_function(Guard) ->
  case Guard(S) of
    true  -> convert(Rules, S);
    false -> convert(Rules, Def)
  end;
convert([_|Rules], S) -> convert(Rules, S).
