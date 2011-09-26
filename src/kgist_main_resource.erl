-module(kgist_main_resource).

%%% Exports ====================================================================
-export([ init/1
        , to_html/2
        , content_types_provided/2
        ]).

%%% Includes ===================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%% Code =======================================================================
%%% Callbacks ------------------------------------------------------------------
init(X) ->
  io:format("Main init: ~p~n", [X]),
  {ok, undefined}.
    
content_types_provided(ReqData, Context) ->
  {[{"text/html", to_html}], ReqData, Context}.

to_html(ReqData, Context) ->
  Path = wrq:disp_path(ReqData),

  %% io:format("Main ReqData: ~p~n", [ReqData]),
  io:format("Main Path: ~p~n", [Path]),
  io:format("Main Context: ~p~n", [Context]),

  HBody = io_lib:format("<html><body>MAIN</body></html>~n",
                        []),
  {HBody, ReqData, Context}.
