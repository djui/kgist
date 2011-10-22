-module(kgist).

%%% Exports ====================================================================
-export([ start/0
        , stop/0
        ]).

-export([ convert_id/1
        , expired/1
        , expires/1
        ]).

%%% Imports ====================================================================
-import(tulib_application, [ ensure_started/1 ]).
-import(tulib_calendar, [ unix_ts/0
                        , unix_ts/1
                        , unix_ts_to_datetime/1
                        ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("tulib/include/tulib_calendar.hrl").

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
start() ->
  ensure_started(mnesia),
  kgist_db:ensure_initialized(),
  ensure_started(misultin),
  application:start(kgist).

stop() ->
  Res = application:stop(kgist),
  application:stop(misultin),
  application:stop(mnesia),
  Res.

convert_id(Id) ->
  case string:to_integer(Id) of
    {error,   _} -> error;
    {GistId, []} ->
      case GistId >= 1000 andalso GistId =< 9999 of
        true  -> {ok, GistId};
        false -> error
      end
  end.

expired(Gist) when is_record(Gist, gist) -> unix_ts() >= expires(Gist).

expires(#gist{creation_time=CT, expires="1h"}) -> unix_ts_to_datetime(CT+?A_HOUR );
expires(#gist{creation_time=CT, expires="1d"}) -> unix_ts_to_datetime(CT+?A_DAY  );
expires(#gist{creation_time=CT, expires="1w"}) -> unix_ts_to_datetime(CT+?A_WEEK );
expires(#gist{creation_time=CT, expires="1m"}) -> unix_ts_to_datetime(CT+?A_MONTH);
expires(#gist{creation_time=CT, expires="1y"}) -> unix_ts_to_datetime(CT+?A_YEAR );
expires(#gist{             expires=undefined}) -> undefined.
