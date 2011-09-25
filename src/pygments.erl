-module(pygments).

-export([ pygmentize/1
        ]).

pygmentize(_Source) ->
  nyi.

os_cmd(Cmd) ->
  try
    Port = erlang:open_port({spawn_command, Cmd}, [exit_status]),
    os_cmd_loop(Port, [])
  catch
    _C:R ->
      {error, R}
  end.

os_cmd_loop(Port, Data0) ->
  receive
    {Port, {data, Data}} ->
      os_cmd_loop(Port, Data0 ++ Data);
    {Port, {exit_status, Status}} ->
      {ok, Status};
    {'EXIT', Port, Reason} ->
      {error, Reason}
  end.
