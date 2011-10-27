-module(kgist_gist).

%%% Exports ====================================================================
-export([ convert_id/1
	, defaults/1
        , expired/1
        , expires/1
        , random_id/0
        , rel_expiry_date/1
        , sanitize/1
        , to_dict/1
        , to_plist/1
        ]).

%%% Imports ====================================================================
-import(tulib_calendar, [ unix_ts/0
                        , unix_ts/1
                        , unix_ts_to_datetime/1
                        ]).
-import(tulib_erlang,   [ len/1 ]).
-import(tulib_string,   [ fmt/2
                        , s/1
                        ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("tulib/include/tulib_calendar.hrl").

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
convert_id(Id) when is_integer(Id) ->
  case Id >= 1000 andalso Id =< 9999 of
    true  -> {ok,    Id};
    false -> {error, out_of_bound}
  end;
convert_id(Id0) ->
  case string:to_integer(Id0) of
    {error, _ } -> {error, invalid_format};
    {Id,    []} -> convert_id(Id)
  end.

random_id() -> random:uniform(9000)+999. %% 1000 <= Id <= 9999.

expired(Gist) when is_record(Gist, gist) -> unix_ts() >= expires(Gist).

expires(#gist{creation_time=CT, expires="1h"}) -> unix_ts_to_datetime(CT+?A_HOUR );
expires(#gist{creation_time=CT, expires="1d"}) -> unix_ts_to_datetime(CT+?A_DAY  );
expires(#gist{creation_time=CT, expires="1w"}) -> unix_ts_to_datetime(CT+?A_WEEK );
expires(#gist{creation_time=CT, expires="1m"}) -> unix_ts_to_datetime(CT+?A_MONTH);
expires(#gist{creation_time=CT, expires="1y"}) -> unix_ts_to_datetime(CT+?A_YEAR );
expires(#gist{             expires=undefined}) -> undefined.

rel_expiry_date(Gist) when is_record(Gist, gist) ->
  rel_expiry_date(Gist#gist.creation_time, Gist#gist.expires).

%% @doc Sanitize input ETL style.
sanitize(Vals) ->
  %% Extract
  Author0  = proplists:get_value("author",      Vals, ""),
  Code0    = proplists:get_value("code",        Vals, ""),
  Desc0    = proplists:get_value("description", Vals, ""),
  Expires0 = proplists:get_value("expires",     Vals, ""),
  Irc0     = proplists:get_value("irc",         Vals, ""),
  File0    = proplists:get_value("filename",    Vals, ""),
  Lang0    = proplists:get_value("language",    Vals, ""),
  %% Translate
  Author   = conv([{set, "anonymous"}, {max, 32}], Author0),
  Code     = conv([{max, 1048576}], Code0), %% 1 MB
  Expires  = conv([{in, ["1h","1d","1w","1m","1y"]}], Expires0),
  Desc     = conv([{max, 1024}], Desc0),
  Lang     = conv([{fun pygments:lang_exists/1, "Text only"}], Lang0),
  DefFile  = fmt("gistfile~s", [pygments:lang_ext(Lang)]),
  File     = conv([{set, DefFile}], File0),
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

defaults(Gist=#gist{description=""}) when is_record(Gist, gist) ->
    defaults(Gist#gist{description="-"});
defaults(Gist=#gist{creation_time=CT0}) when is_record(Gist, gist) andalso
					    is_integer(CT0) ->
    {{Y,M,D},{H,I,S}} = unix_ts_to_datetime(CT0),
    CT = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
		       [Y,M,D,H,I,S]),
    defaults(Gist#gist{creation_time=CT});
defaults(Gist) when is_record(Gist, gist) -> Gist.

to_dict(Gist) when is_record(Gist, gist) ->
  dict:from_list(to_plist(Gist)).

to_plist(Gist) when is_record(Gist, gist) ->
  Ks        = record_info(fields, gist),
  [gist|Vs] = tuple_to_list(Gist),
  lists:zip(Ks, Vs).

%%% Internals ------------------------------------------------------------------
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

rel_expiry_date(TS, "1h")      -> do_rel_expiry_date(TS, ?A_HOUR  );
rel_expiry_date(TS, "1d")      -> do_rel_expiry_date(TS, ?A_DAY   );
rel_expiry_date(TS, "1w")      -> do_rel_expiry_date(TS, ?A_WEEK  );
rel_expiry_date(TS, "1m")      -> do_rel_expiry_date(TS, ?A_MONTH );
rel_expiry_date(TS, "1y")      -> do_rel_expiry_date(TS, ?A_YEAR  );
rel_expiry_date(TS, undefined) -> do_rel_expiry_date(TS, undefined).

do_rel_expiry_date(_,         undefined) -> "never";
do_rel_expiry_date(Timestamp, RelAmount) ->
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
