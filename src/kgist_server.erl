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
  Body       = kgist_view:render(gist_new, ViewCtx),
  Req:ok(?HTML, Body);

handle('POST', ["gist"], Req) ->
  Id         = kgist_db:next_id(),
  %% TODO Just do a redirect to 'PUT gist/Id'?
  Vals       = Req:parse_post(),
  {ok, Gist} = spec(Vals),
  Req2       = Req:set_cookie("author", Gist#gist.author),
  ok         = kgist_db:put(Id, Gist),
  redirect(["gist", Id], Req);

handle('GET', ["gist", GistId0], Req) ->
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
  Body    = kgist_view:render(gist_view, ViewCtx),
  Req:ok(?HTML, Body);

handle('GET', ["gist", GistId0, "raw"], Req) ->
  {ok, GistId} = kgist:convert_id(GistId0),
  {ok, Gist}   = kgist_db:get(GistId),
  Req:ok(?PLAIN, Gist#gist.code);

handle('GET', ["gist", GistId0, "download"], Req) ->
  {ok, GistId} = kgist:convert_id(GistId0),
  {ok, Gist}   = kgist_db:get(GistId),
  Header       = add_header_download(Gist#gist.filename, ?PLAIN),
  Req:ok(Header, Gist#gist.code);

handle('GET', Path, Req) -> %% Serve static files
  Req:file(attachment, file_path(root_dir(), Path));

handle(_, _, Req) -> %% Nothing matched
  '404'(Req).

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

redirect(Path, Req) -> '303'(Path, Req).

'303'(Path, Req) ->
  %% TODO Construct path and set correct header
  Header = [{'Path'}, filename:join(Path)],
  Req:respond(303, Header, "").
  
'404'(Req) ->
  Header = [{'Content-Type', "text/html"}],
  Req:respond(404, Header, "File not Found.").
