-module(kgist_sup).
-behaviour(supervisor).

-export([ start_link/0
        , init/1
        ]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Webmachine = ?CHILD(webmachine, worker),
  Pygments   = ?CHILD(pygments, worker),
  Mustache   = ?CHILD(mustache, worker),
  {ok, {{one_for_one, 5, 10},
        [ Webmachine
        , Pygments
        , Mustache
        ]}.

init([]) ->
  Host = case application:get_env(host) of
           undefined -> "0.0.0.0";
           Defined  -> Defined
         end,
  Port = case application:get_env(port) of
           undefined -> "8080";
           Defined   -> Defined
         end,
  {ok, Dispatch} = file:consult(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "dispatch.conf"])),
  WebConfig = [ {ip, Host}
              , {port, Port}
              , {log_dir, "priv/log"}
              , {dispatch, Dispatch}
              ],
  Web = {webmachine_mochiweb, {webmachine_mochiweb, start, [WebConfig]}, permanent, 5000, worker, dynamic},
  Processes = [Web],
  {ok, {{one_for_one, 10, 10}, Processes}}.
