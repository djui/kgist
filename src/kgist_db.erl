-module(kgist_db).

%%% Compiles ===================================================================
-compile({no_auto_import, [ get/1
                          , put/2
                          ]}).

%%% Exports ====================================================================
-export([ archive/1
        , backup/1
        , ensure_initialized/0
        , exists/1
        , get/1
        , get_rel_since/1
        , get_since/1
        , migrate/0
        , next_id/0
        , put/2
        , recents/1
        ]).

%%% Imports ====================================================================
-import(tulib_calendar, [ unix_ts/0
                        , unix_ts/1
                        ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("tulib/include/tulib_calendar.hrl").

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
backup(Filename) ->
  mnesia:backup(Filename).

ensure_initialized() ->
  Node    = node(),
  Tables  = [{?GIST_TABLE, [ {disc_copies, [Node]}
                           , {attributes, record_info(fields, gist)}
                           , {type, set} 
                           ]}
            ],
  ensure_schema(Node),
  ensure_tables(Tables).

ensure_schema(Node) ->
  case mnesia:table_info(schema, disc_copies) of
    [] -> %% Schema not yet a persistent
      case mnesia:change_table_copy_type(schema, Node, disc_copies) of
        {aborted, {already_exists, schema, Node, disc_copies}} ->
          ok;
        {atomic, ok} ->
          ok
      end;
    [Node] -> %% Schema is local and persistent
      ok;
    [_] -> %% DB was copy, node name, or hostname has changed
      {ok, DBDir} = application:get_env(mnesia, dir),
      migrate_schema(DBDir)
  end.

ensure_tables(Tables) ->
  Create = fun({TableName, TableDef}) ->
               case mnesia:create_table(TableName, TableDef) of
                 {aborted, {already_exists, TableName}} ->
                   ok;
                 {atomic, ok} ->
                   ok
               end
           end,
  lists:foreach(Create, Tables).

exists(Id) ->
  case get(Id) of
    {ok,       Gist    } -> {true,  Gist    };
    {archived, _Gist   } -> {false, archived};
    {expired,  _Gist   } -> {false, expired };
    {error,    notfound} -> {false, notfound}
  end.

get(Id) ->
  case mnesia:dirty_read(?GIST_TABLE, Id) of
    {aborted, Err} -> {error, Err};
    []             -> {error, notfound};
    [Gist] when Gist#gist.archived -> {archived, Gist};
    [Gist] ->
      case kgist_gist:expired(Gist) of
        true  -> {expired, Gist};
        false -> {ok,      Gist}
      end
  end.

get_rel_since(RelTime) ->
  Now      = unix_ts(),
  AbsTime = Now - RelTime,
  get_since(AbsTime).

get_since({_,_,_}=D) -> get_since(unix_ts(D));
get_since(SinceTS) ->
  MatchHead = #gist{ creation_time='$1'
                   , archived=false
                   , _= '_'
                   },
  Guards    = [{'>=', '$1', SinceTS}],
  Result    = ['$_'],
  Res = mnesia:dirty_select(?GIST_TABLE, [{MatchHead, Guards, Result}]),
  lists:map(fun kgist_gist:defaults/1, 
	    lists:reverse(lists:keysort(#gist.creation_time, Res))).

migrate() ->
  {ok, DBDir} = application:get_env(mnesia, dir),
  %% Migrate old node.js database
  maybe_migrate_from_node(filename:join([DBDir, "gist.db"])),
  %% ...
  ok.

next_id() -> %% HACK! Random non-collision recursion
  NextId = kgist_gist:random_id(),
  case mnesia:dirty_read(?GIST_TABLE, NextId) of
    []  -> NextId;
    [_] -> next_id()
  end.

archive(Gist) when is_record(Gist, gist) ->
  put(Gist#gist.id, Gist#gist{archived=true});
archive(Id) ->
  {ok, Gist} = get(Id),
  archive(Gist).

put(Id, Gist0) ->
  Gist = Gist0#gist{id=Id},
  case mnesia:dirty_write(?GIST_TABLE, Gist) of
    {aborted, Err} -> {error, Err};
    ok             -> ok
  end.

recents(N) ->
  MatchHead = #gist{ creation_time='$1'
                   , archived=false
                   , _= '_'
                   },
  Guards    = [],
  Result    = ['$_'],
  Res = mnesia:dirty_select(?GIST_TABLE, [{MatchHead, Guards, Result}]),
  lists:map(fun kgist_gist:defaults/1, 
	    lists:reverse(
	      lists:sublist(
		lists:keysort(#gist.creation_time, Res), N))).

%%% Internals ------------------------------------------------------------------
migrate_schema(DBDir) when is_list(DBDir) ->
  %% Not really rock-solid at the moment
  ignore;
migrate_schema(DBDir) ->
  NewNode = node(),
  Restart = mnesia:system_info(is_running),
  stopped = mnesia:stop(),
  SchemaF = filename:join([DBDir, "schema.DAT"]),
  true    = dets:is_dets_file(SchemaF),
  {ok, D} = dets:open_file(schema, [{file,SchemaF},{repair,false},{keypos,2}]),
  Schema  = dets:match_object(D, {schema, '_', '_'}),
  ok      = dets:delete_all_objects(D),
  Convert =
    fun({schema, Tab, Val0}) ->
        Val1 = orddict:update(cookie, fun({Cookie, _OldNode}) ->
                                          {Cookie, NewNode}
                                      end, Val0),
        Val2 = orddict:update(version, fun({Version, []}) ->
                                           {Version, []};
                                          ({Version, {_OldNode, Cookie}}) ->
                                           {Version, {NewNode, Cookie}}
                                       end, Val1),
        Val3 = orddict:update(ram_copies, fun([])       -> [];
                                             (_OldNode) -> NewNode
                                          end, Val2),
        Val4 = orddict:update(disc_copies, fun([])       -> [];
                                              (_OldNode) -> NewNode
                                           end, Val3),
        Val5 = orddict:update(disc_only_copies, fun([])       -> [];
                                                   (_OldNode) -> NewNode
                                                end, Val4),
        true = dets:insert_new(D, {schema, Tab, Val5})
    end,
  lists:foreach(Convert, Schema),
  ok      = dets:close(),
  Restart andalso mnesia:start().

maybe_migrate_from_node(DBFilename) ->
  case filelib:is_file(DBFilename) of
    true  -> migrate_from_node(DBFilename);
    false -> ignore
  end.

migrate_from_node(DBFilename) ->
  {ok, File} = file:read_file(DBFilename),
  Lines = string:tokens(binary_to_list(File), "\n"),
  MigrateFun =
    fun(Line) ->
        {struct, [ {<<"key">>, _Key}
                 , {<<"val">>, {struct, Gist}}
                 ]} = mochijson2:decode(Line),
        Desc0     = from_binary(proplists:get_value(<<"description">>, Gist)),
        Filename0 = from_binary(proplists:get_value(<<"filename">>, Gist)),
        Language0 = from_binary(proplists:get_value(<<"language">>, Gist)),
        Code0     = from_binary(proplists:get_value(<<"code">>, Gist)),
        Author0   = from_binary(proplists:get_value(<<"author">>, Gist)),
        IRC0      = from_binary(proplists:get_value(<<"irc">>, Gist)),
        Expires0  = from_binary(proplists:get_value(<<"expires">>, Gist)),
        HlCode0   = from_binary(proplists:get_value(<<"hl_code">>, Gist)),
        CTime0    = from_binary(proplists:get_value(<<"ctime">>, Gist)),
        Id0       = from_binary(proplists:get_value(<<"id">>, Gist)),
        Archived0 = from_binary(proplists:get_value(<<"archived">>, Gist)),
        
        Desc     = Desc0,
        Filename = Filename0,
        Language = Language0,
        Code     = Code0,
        Author   = Author0,
        IRC      = case IRC0 of
                     undefined -> "";
                     <<>> -> "";
                     _ -> IRC0
                   end,
        Expires  = case Expires0 of
                     "" -> undefined;
                     _ -> Expires0
                   end,
        HlCode   = HlCode0,
        CTime    = CTime0 div 1000, % ms -> s
        Id       = list_to_integer(Id0),
        Archived = case Archived0 of
                     undefined -> false;
                     _ -> Archived0
                   end,
        
        NewGist = #gist{ id = Id
                       , creation_time = CTime
                       , expires = Expires
                       , archived = Archived
                       , language = Language
                       , author = Author
                       , filename = Filename
                       , description = Desc
                       , irc = IRC
                       , code = Code
                       , code_highlighted = HlCode
                       },
        mnesia:dirty_write(NewGist)
    end,

lists:foreach(MigrateFun, Lines).

from_binary(B) when is_binary(B) -> erlang:binary_to_list(B);
from_binary(T) -> T.
