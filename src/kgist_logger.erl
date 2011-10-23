%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Uwe Dauernheim <uwe@dauernheim.net>
%% @copyright 2007-2008 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
-module(kgist_logger).
-behaviour(gen_server).

-author("Justin Sheehy <justin@basho.com>").
-author("Andy Gross <andy@basho.com>").
-author("Uwe Dauernheim <uwe@dauernheim.net>").

%%% Exports ====================================================================
-export([ access_log/1
        , start_link/1
        ]).
-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , refresh/0
        , terminate/2
        ]).

%%% Records ====================================================================
-record(state, { filename
               , handle
               , hourstamp
               }).

%%% Defines ====================================================================
-define(SERVER, ?MODULE).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
start_link(Config) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

access_log(Res) ->
  gen_server:cast(?SERVER, {log_access, Res}).

%%% Callbacks ------------------------------------------------------------------
init(Config) ->
  defer_refresh(),
  BaseDir  = proplists:get_value(log_dir, Config),
  Filename = accesslog_path(BaseDir),
  ok       = filelib:ensure_dir(Filename),
  DateHour = datehour(),
  Handle   = log_open(Filename, DateHour),
  {ok, #state{filename=Filename, handle=Handle, hourstamp=DateHour}}.

handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

handle_cast({log_access, Res}, State) ->
  NewState = maybe_rotate(State, now()),
  Msg      = format_res(Res),
  log_write(NewState#state.handle, Msg),
  {noreply, NewState};
handle_cast({refresh, Time}, State) ->
  {noreply, maybe_rotate(State, Time)};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  terminated.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internals ------------------------------------------------------------------
accesslog_path(BaseDir) ->
  filename:join(BaseDir, "access.log").

format_res({PeerAddr, DateTime, RequestLine, HttpCode, ContentLength}) ->
  Fmt  = "~s - - [~s] \"~s\" ~p ~p~n",
  Data = [PeerAddr, DateTime, RequestLine, HttpCode, ContentLength],
  io_lib:format(Fmt, Data).

maybe_rotate(State, Time) ->
  ThisHour = datehour(Time),
  case ThisHour =:= State#state.hourstamp of
    true  -> State;
    false ->
      defer_refresh(),
      log_close(State#state.handle),
      Handle = log_open(State#state.filename, ThisHour),
      State#state{hourstamp=ThisHour, handle=Handle}
  end.    

log_open(FileName, DateHour) ->
  LogName  = FileName ++ suffix(DateHour),
  {ok, FD} = file:open(LogName, [read, write, raw]),
  ok       = fix_log(FD),
  ok       = file:truncate(FD),
  {?MODULE, LogName, FD}.

log_write({?MODULE, _, FD}, IoData) -> file:write(FD, lists:flatten(IoData)).

log_close({?MODULE, _, FD}) -> file:close(FD).

defer_refresh() ->
  {_, {_, M, S}} = calendar:universal_time(),
  Time = 1000 * (3600 - ((M * 60) + S)), %% +1 minute
  timer:apply_after(Time, ?MODULE, refresh, []).

refresh() -> refresh(now()).

refresh(Time) -> gen_server:cast(?MODULE, {refresh, Time}).

datehour() -> datehour(now()).

datehour(Now) ->
  {{Y, M, D}, {H, _, _}} = calendar:now_to_universal_time(Now),
  {Y, M, D, H}.

suffix({Y, M, D, H}) ->
  YS = zeropad(Y, 4),
  MS = zeropad(M, 2),
  DS = zeropad(D, 2),
  HS = zeropad(H, 2),
  lists:flatten([$., YS, $_, MS, $_, DS, $_, HS]).

zeropad_str(NumStr, Zeros) when Zeros > 0 -> zeropad_str([$0|NumStr], Zeros-1);
zeropad_str(NumStr, _)                    -> NumStr.

zeropad(Num, MinLength) ->
  NumStr = integer_to_list(Num),
  zeropad_str(NumStr, MinLength - length(NumStr)).

%% @doc Seek backwards to the last valid log entry. Valid means: has ending new
%% line.
fix_log(FD) ->
  {ok, Location} = file:position(FD, eof),
  fix_log(FD, Location).
  
fix_log(_FD, 0) -> ok;
fix_log(FD,  1) -> {ok, 0} = file:position(FD, 0), ok;
fix_log(FD, Location) ->
  case file:pread(FD, Location - 1, 1) of
    {ok, [$\n | _]} -> ok;
    {ok, _}         -> fix_log(FD, Location - 1)
  end.
