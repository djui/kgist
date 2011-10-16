-module(kgist_logger).
-behaviour(gen_server).

%%% Exports ====================================================================
-export([ start_link/1
        , log/1
        ]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%% Records ====================================================================
-record(state, { log_dir }).

%%% Defines ====================================================================
-define(SERVER, ?MODULE).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
start_link(Config) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

log(Res) ->
  gen_server:cast(?SERVER, {log, Res}).

%%% Callbacks ------------------------------------------------------------------
init(Config) ->
  LogDir = proplists:get_value(log_dir, Config),
  {ok, #state{log_dir=filename:absname(LogDir)}}.

handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

handle_cast({log, Res}, State=#state{log_dir=_LogDir}) ->
  Headers = Res:get(headers),
  UserAgent = misultin_utility:header_get_value('User-Agent', Headers),
  Referer = case misultin_utility:header_get_value('Referer', Headers) of
              false -> "";
              R     -> R
            end,
  Status = 200,  %% TODO
  Length = 1234, %% TODO
  log(Res:get(peer_addr),
      Res:get(method),
      Res:get(uri_unquoted),
      Res:get(vsn),
      Status,
      Length,
      Referer,
      UserAgent),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

log(IP, Method, Path, {VM,Vm}, Status, Length, Referrer, UserAgent) ->
  F = fmt(IP, Method, Path, {VM,Vm}, Status,  Length, Referrer, UserAgent),
  error_logger:info_msg(F). %% TODO Log to disk

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  terminated.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internals ------------------------------------------------------------------
fmt(IP, Method, Path, {VM,Vm}, Status,  Length, Referrer, UserAgent) ->
  io_lib:format("~s - - ~s \"~s ~s HTTP/~B.~B\" ~B ~B \"~s\" \"~s\"\n",
                [ ip(IP)
                , fmtnow()
                , Method
                , Path
                , VM, Vm
                , Status
                , Length
                , Referrer
                , UserAgent
                ]).

ip(IP) when is_tuple(IP) -> inet_parse:ntoa(IP);
ip(undefined)            -> "0.0.0.0";
ip(HostName)             -> HostName.

fmtnow() ->
  {{Year, Month, Date}, {Hour, Min, Sec}} = calendar:local_time(),
  io_lib:format("~2..0w/~2..0w/~4..0w:~2..0w:~2..0w:~2..0w ~s",
                [Date, Month, Year, Hour, Min, Sec, zone()]).

zone() ->
  Time = erlang:universaltime(),
  LocalTime = calendar:universal_time_to_local_time(Time),
  DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
    calendar:datetime_to_gregorian_seconds(Time),
  zone((DiffSecs/3600)*100).

zone(Val) when Val < 0  -> io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 -> io_lib:format("+~4..0w", [trunc(abs(Val))]).
