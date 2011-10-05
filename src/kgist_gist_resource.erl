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
-import(tulib_calendar, [unix_timestamp/0
                        ,unix_timestamp/1
                        ]).

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
      %% TODO set redirect to serve new gist
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
  Recents = kgist_view:to_list(kgist_db:recents()),
  RelDate = rel_date(Gist#gist.creation_time, Gist#gist.expires),
  %% TODO Exclude RelDate =:= undefined gists and remove&redirect them
  ViewCtx = [ {page_title,       "Gist " ++ integer_to_list(Gist#gist.id)}
            , {gists,            Recents}
            , {gist_id,          Gist#gist.id}
            , {gist_description, Gist#gist.description}
            , {gist_author,      Gist#gist.author}
            , {gist_filename,    Gist#gist.filename}
            , {gist_language,    Gist#gist.language}
            , {gist_hlcode,      Gist#gist.code_highlighted}
            , {gist_expires,     RelDate}
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
  Gist = #gist{ creation_time    = unix_timestamp()
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

rel_date(Timestamp, "1h")      -> do_rel_date(Timestamp, 60*60);
rel_date(Timestamp, "1d")      -> do_rel_date(Timestamp, 60*60*24);
rel_date(Timestamp, "1w")      -> do_rel_date(Timestamp, 60*60*24*7);
rel_date(Timestamp, "1m")      -> do_rel_date(Timestamp, 60*60*24*30);
rel_date(Timestamp, "1y")      -> do_rel_date(Timestamp, 60*60*24*365);
rel_date(Timestamp, undefined) -> do_rel_date(Timestamp, undefined).

do_rel_date(_,         undefined) -> "never";
do_rel_date(Timestamp, RelAmount) ->
  Diff = (Timestamp + RelAmount) - unix_timestamp(),
  io:format("Diff: ~p~n", [Diff]),
  case Diff of
    D when D > 60*60*24*7*30 ->
      Months = D div (60*60*24*7*30),
      "in " ++ integer_to_list(Months) ++ " month" ++ plural(Months);
    D when D > 60*60*24*7 ->
      Weeks = D div (60*60*24*7),
      "in " ++ integer_to_list(Weeks) ++ " week" ++ plural(Weeks);
    D when D > 60*60*24 ->
      Days = D div (60*60*24),
      "in " ++ integer_to_list(Days) ++ " day" ++ plural(Days);
    D when D > 60*60 ->
      Hours = D div (60*60),
      "in " ++ integer_to_list(Hours) ++ " hour" ++ plural(Hours);
    D when D > 60 ->
      Minutes = D div 60,
      "in " ++ integer_to_list(Minutes) ++ " minute" ++ plural(Minutes);
    D when D > 0 ->
      Seconds = D,
      "in " ++ integer_to_list(Seconds) ++ " second" ++ plural(Seconds);
    D when D =:= 0 ->
      "now";
    D when D < 0 ->
      undefined
  end.

plural(N) when N > 0   -> "s";
plural(N) when N =:= 0 -> "".
