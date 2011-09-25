-module(kgist_app).
-behaviour(application).

%%% Exports ====================================================================
-export([ start/2
        , stop/1
        ]).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
  kgist_sup:start_link().

stop(_State) ->
  ok.
