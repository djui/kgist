-module(kgist_server).

%%% Exports ====================================================================
-export([ handle_http/1 ]).

%%% Imports ====================================================================
-import(tulib_calendar, [ unix_ts/0
                        , unix_ts/1
                        , unix_ts_to_datetime/1
                        ]).
-import(tulib_erlang,   [ len/1 ]).
-import(tulib_string,   [ fmt/2 ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("tulib/include/tulib_calendar.hrl").

%%% Defines ====================================================================
-define(HTML,  [{'Content-Type', "text/html" }]).
-define(PLAIN, [{'Content-Type', "text/plain"}]).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
handle_http(Req) ->
  Method = Req:get(method),
  Res    = Req:resource([lowercase, urldecode]),
  handle(Method, Res, Req).

%%% Dispatches ------------------------------------------------------------------
handle('GET', [], Req) -> %% Redirect
  handle('GET', ["gist"], Req);
handle('GET', ["gist"], Req) -> %% New gist
  Recents    = kgist_view:to_list(kgist_db:recents()),
  Author     = get_cookie_value("author", "", Req),
  {ok, Lang} = application:get_env(default_language),
  ViewCtx    = [ {page_title,    ""     }
               , {gists,         Recents}
               , {gist_author,   Author }
               , {gist_language, Lang   }
               ],
  HBody      = kgist_view:render(gist_new, ViewCtx),
  Req:ok(?HTML, HBody);
handle('GET', ["gist", GistId], Req) ->
  Req:ok(?HTML, "Gist Id ~s", [GistId]);
handle('GET', ["gist", GistId, "raw"], Req) ->
  Req:ok(?PLAIN, "raw ~s", [GistId]);
handle('GET', ["gist", GistId, "download"], Req) ->
  Header = add_header_download(GistId, []),
  Req:ok(Header, "download ~s", [GistId]);
handle('GET', Path, Req) -> %% Serve static files
  Req:file(attachment, file_path(root_dir(), Path));
handle(_, _, Req) -> %% Nothing matched
  '404'(Req).

%% Args = Req:parse_qs(),
%% Req:get_variable("value", Args),
%% Req:parse_post(),

expired(Gist) -> unix_ts() >= expires(Gist). %% true = N >= undefined

%% @callback webmachine
expires(ReqData, Ctx=#ctx{rsrc=Gist}) -> {expires(Gist), ReqData, Ctx}.

expires(#gist{creation_time=CTime, expires="1h"}) -> unix_ts_to_datetime(CTime + ?A_HOUR );
expires(#gist{creation_time=CTime, expires="1d"}) -> unix_ts_to_datetime(CTime + ?A_DAY  );
expires(#gist{creation_time=CTime, expires="1w"}) -> unix_ts_to_datetime(CTime + ?A_WEEK );
expires(#gist{creation_time=CTime, expires="1m"}) -> unix_ts_to_datetime(CTime + ?A_MONTH);
expires(#gist{creation_time=CTime, expires="1y"}) -> unix_ts_to_datetime(CTime + ?A_YEAR );
expires(#gist{expires=undefined})                 -> undefined.
%%expires(undefined)                                -> undefined.

%% @callback content_types_accepted
from_form(ReqData, Ctx) ->
  Id      = Ctx#ctx.id,
  ReqBody = wrq:req_body(ReqData),
  Vals    = mochiweb_util:parse_qs(ReqBody),
  {ok, Gist} = spec(Vals),
  ReqData2 = set_cookie("author", Gist#gist.author, ReqData),
  case kgist_db:put(Id, Gist) of
    {error, Err} -> {Err, ReqData2, Ctx};
    ok ->
      ReqData3 = wrq:do_redirect(true, ReqData2),
      {true, ReqData3, Ctx}
  end.

%% @callback webmachine
post_is_create(ReqData, Ctx) ->
  ReqBody = wrq:req_body(ReqData),
  Vals    = mochiweb_util:parse_qs(ReqBody),
  %% TODO Not strictly, combine with: resource_exists
  {true, ReqData, Ctx}.

%% @callback webmachine
previously_existed(ReqData, Ctx=#ctx{rsrc=notfound}) -> {false, ReqData, Ctx};
previously_existed(ReqData, Ctx=#ctx{rsrc=archived}) -> {true,  ReqData, Ctx};
previously_existed(ReqData, Ctx=#ctx{rsrc=expired }) -> {true,  ReqData, Ctx};
previously_existed(ReqData, Ctx)                     -> {true,  ReqData, Ctx}.

process_post(ReqData0, Ctx) ->
  %% TODO Convert hidden _method delete to DELETE
  ReqBody = wrq:req_body(ReqData),
  Vals    = mochiweb_util:parse_qs(ReqBody),

  ReqData = case wrq:get_qs_value("_method", ReqData0) of
              undefined -> wrq:method(ReqData0);
              M         -> wrq:set_method(string:to_upper(M), ReqData0)
            end,
  {true, ReqData, Ctx}.

%% @callback webmachine
resource_exists(ReqData, Ctx) ->
  case gist_id(ReqData) of
    error        -> {false, ReqData, Ctx};
    {ok, GistId} ->
      case kgist_db:get(GistId) of
        {ok,    Gist    } -> {true,  ReqData, Ctx#ctx{rsrc=Gist}};
        {error, notfound} -> {false, ReqData, Ctx#ctx{rsrc=notfound}};
        {error, archived} -> {false, ReqData, Ctx#ctx{rsrc=archived}};
        {error, expired } -> {false, ReqData, Ctx#ctx{rsrc=expired}}
      end
  end.

%% @callback content_types_provided
to_text(ReqData0, Ctx=#ctx{action=download, rsrc=Gist}) ->
  ReqData = attachment(Gist#gist.filename, ReqData0),
  Text    = Gist#gist.code,
  {Text, ReqData, Ctx};
to_text(ReqData, Ctx=#ctx{rsrc=Gist}) ->
  Text = Gist#gist.code,
  {Text, ReqData, Ctx}.

%% @callback content_types_provided
to_html(ReqData, Ctx=#ctx{action=raw})      -> to_text(ReqData, Ctx);
to_html(ReqData, Ctx=#ctx{action=download}) -> to_text(ReqData, Ctx);
to_html(ReqData, Ctx=#ctx{rsrc=Gist})   ->
  Recents = kgist_view:to_list(kgist_db:recents()),
  RelDate = rel_date(Gist#gist.creation_time, Gist#gist.expires),
  ViewCtx = [ {page_title,       fmt("Gist ~b", [Gist#gist.id])}
            , {gists,            Recents                   }
            , {gist_author,      Gist#gist.author          }
            , {gist_description, Gist#gist.description     }
            , {gist_expires,     RelDate                   }
            , {gist_filename,    Gist#gist.filename        }
            , {gist_hlcode,      Gist#gist.code_highlighted}
            , {gist_id,          Gist#gist.id              }
            , {gist_language,    Gist#gist.language        }
            ],
  HBody   = kgist_view:render(gist_view, ViewCtx),
  {HBody, ReqData, Ctx}.

%% @doc Dispatch callback guard
valid_gist_id(ReqData) -> gist_id(ReqData) =/= error.

%%% Internals ------------------------------------------------------------------
add_header(K, V, H) ->
  [{K,V}|H].

add_header_download(File, H) ->
  add_header('Content-Disposition', fmt("attachment; filename=~s", [File]), H).

get_cookie_value(Key, Def, Req) ->
  Cookies = Req:get_cookies(),
  Cookie  = Req:get_cookie_value(Key, Cookies),
  case Cookie of
    undefined -> Def;
    C         -> C
  end.

file_path(AbsRoot, RelPath0) ->
  RelPath = lists:filter(fun("..") -> false; (_) -> true end, RelPath0),
  filename:join([AbsRoot|RelPath]).

root_dir() ->
  {ok, RootDir} = application:get_env(kgist, root_dir),
  filename:absname(RootDir).

'404'(Req) ->
 Header = [{'Content-Type', "text/html"}],
 Req:respond(404, Header, "File not Found.").

gist_id(ReqData) ->
  Id = wrq:path_info(id, ReqData),
  case string:to_integer(Id) of
    {error,   _} -> error;
    {GistId, []} ->
      case GistId >= 1000 andalso GistId =< 9999 of
        true  -> {ok, GistId};
        false -> error
      end
  end.

attachment(Filename, ReqData) ->
  wrq:set_resp_header("content-disposition",
                      fmt("attachment; filename=~s", [Filename]),
                      ReqData).

spec(Vals) ->
  %% ETL Style
  %% Extract
  Author0  = proplists:get_value("author",      Vals, ""),
  Code0    = proplists:get_value("code",        Vals, ""),
  Desc0    = proplists:get_value("description", Vals, ""),
  Expires0 = proplists:get_value("expires",     Vals, ""),
  Irc0     = proplists:get_value("irc",         Vals, ""),
  File0    = proplists:get_value("filename",    Vals, ""),
  Lang0    = proplists:get_value("language",    Vals, ""),
  %% Translate
  Author   = conv([{non_empty, "anonymous"}, {max, 32}], Author0),
  Code     = conv([{max, 1048576}], Code0), %% 1 MB
  Expires  = conv([{in, ["1h","1d","1w","1m","1y"]}], Expires0),
  Desc     = conv([{max, 1024}], Desc0),
  Lang     = conv([{fun pygments:lang_exists/1, "Text only"}], Lang0),
  DefFile  = fmt("gistfile~s", [pygments:lang_ext(Lang)]),
  File     = conv([{non_empty, DefFile}], File0),
  Irc      = conv([{min, 2}, fun(["#"|_]) -> true; (_) -> false end], Irc0),
  HlCode   = pygments:pygmentize(Lang, Code),
  %% Load
  Gist = #gist{ creation_time    = unix_ts()
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

conv([],              S) -> S;
conv(_,       undefined) -> undefined;
conv([set      |Rs],  S) -> conv([{non_empty,undefined}|Rs],                 S);
conv([{set,D}  |Rs], "") -> conv(Rs,                                         D);
conv([{min,Len}|Rs],  S) -> conv([fun(E) -> len(E) >= Len end|Rs],           S);
conv([{max,Len}|Rs],  S) -> conv(Rs,                  string:substr(S, 1, Len));
conv([{in,L}   |Rs],  S) -> conv([{in, L, undefined}|Rs],                    S);
conv([{in,L,D} |Rs],  S) -> conv([{fun(E) -> lists:member(E, L) end, D}|Rs], S);
conv([G        |Rs],  S) when is_function(G) -> conv([{G, undefined}|Rs],    S);
conv([{G, D}   |Rs],  S)
  when is_function(G)    ->
  case G(S) of
    true  -> conv(Rs, S);
    false -> conv(Rs, D)
  end;
conv([_        |Rs],  S) -> conv(Rs, S).

rel_date(Timestamp, "1h")      -> do_rel_date(Timestamp, ?A_HOUR  );
rel_date(Timestamp, "1d")      -> do_rel_date(Timestamp, ?A_DAY   );
rel_date(Timestamp, "1w")      -> do_rel_date(Timestamp, ?A_WEEK  );
rel_date(Timestamp, "1m")      -> do_rel_date(Timestamp, ?A_MONTH );
rel_date(Timestamp, "1y")      -> do_rel_date(Timestamp, ?A_YEAR  );
rel_date(Timestamp, undefined) -> do_rel_date(Timestamp, undefined).

do_rel_date(_,         undefined) -> "never";
do_rel_date(Timestamp, RelAmount) ->
  Diff = (Timestamp + RelAmount) - unix_ts(),
  case Diff of
    T when T > ?A_MONTH -> M = T div ?A_MONTH, fmt("in ~b month~s",  [M, s(M)]);
    T when T > ?A_WEEK  -> W = T div ?A_WEEK,  fmt("in ~b week~s",   [W, s(W)]);
    T when T > ?A_DAY   -> D = T div ?A_DAY,   fmt("in ~b day~s",    [D, s(D)]);
    T when T > ?A_HOUR  -> H = T div ?A_HOUR,  fmt("in ~b hour~s",   [H, s(H)]);
    T when T > ?A_MIN   -> I = T div ?A_MIN,   fmt("in ~b minute~s", [I, s(I)]);
    T when T > 0        -> S = T,              fmt("in ~b second~s", [S, s(S)]);
    0                   -> "now";
    T when T < 0        -> undefined
  end.

s(1)                    -> "";
s(N) when is_integer(N) -> "s".

set_cookie(Key, Value, ReqData) ->
  OneYear = 60*60*24*365,
  {K, V} = mochiweb_cookies:cookie(Key, Value, [{max_age, OneYear}]),
  wrq:set_resp_header(K, V, ReqData).
