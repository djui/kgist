-module(kgist_db).

%%% Exports ====================================================================
-export([ ensure_initialized/0
        , migrate/0
        , backup/1
        ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
ensure_initialized() ->
  Node = node(),
  TableName = ?GIST_TABLE,
  TableDef  = [ {disc_copies, [Node]}
              , {attributes, record_info(fields, gist)}
              , {type, set} 
              ],
  case mnesia:change_table_copy_type(schema, Node, disc_copies) of
    {aborted, {already_exists, schema, Node, disc_copies}} ->
      ok;
    {atomic, ok} ->
      ok
  end,
  case mnesia:create_table(TableName, TableDef) of
    {aborted, {already_exists, TableName}} ->
      ok;
    {atomic, ok} ->
      ok
  end.

migrate() ->
 migrate_schema(),
 migrate_from_node().

backup(Filename) ->
  mnesia:backup(Filename).

%%% Internals ------------------------------------------------------------------
migrate_schema() ->
  nyi.

migrate_from_node() ->
  {ok, DBDir} = application:get_env(mnesia, dir),
  {ok, File} = file:read_file(filename:join([DBDir, "gist.db"])),
  Lines = string:tokens(binary_to_list(File), "\n"),
  MigrateFun =
    fun(Line) ->
        {struct, [ {<<"key">>, Key}
                 , {<<"val">>, {struct, Gist}}
                 ]} = mochijson2:decode(Line),
        Description0  = from_binary(proplists:get_value(<<"description">>, Gist)),
        Filename0     = from_binary(proplists:get_value(<<"filename">>, Gist)),
        Language0     = from_binary(proplists:get_value(<<"language">>, Gist)),
        Code0         = from_binary(proplists:get_value(<<"code">>, Gist)),
        Author0       = from_binary(proplists:get_value(<<"author">>, Gist)),
        IRC0          = from_binary(proplists:get_value(<<"irc">>, Gist)),
        Expires0      = from_binary(proplists:get_value(<<"expires">>, Gist)),
        HlCode0       = from_binary(proplists:get_value(<<"hl_code">>, Gist)),
        CreationTime0 = from_binary(proplists:get_value(<<"ctime">>, Gist)),
        Id0           = from_binary(proplists:get_value(<<"id">>, Gist)),
        Archived0     = from_binary(proplists:get_value(<<"archived">>, Gist)),
        
        Description  = Description0,
        Filename     = Filename0,
        Language     = Language0,
        Code         = Code0,
        Author       = Author0,
        IRC          = case IRC0 of
                         undefined -> "";
                         <<>> -> "";
                         _ -> IRC0
                       end,
        Expires      = case Expires0 of
                         "" -> undefined;
                         _ -> Expires0
                       end,
        HlCode       = HlCode0,
        CreationTime = CreationTime0,
        Id           = list_to_integer(Id0),
        Archived     = case Archived0 of
                         undefined -> false;
                         _ -> Archived0
                       end,
        
        NewGist = #gist{ id = Id
                       , creation_time = CreationTime
                       , expires = Expires
                       , archived = Archived
                       , language = Language
                       , author = Author
                       , filename = Filename
                       , description = Description
                       , irc = IRC
                       , code = Code
                       , code_highlighted = HlCode
                       },
        mnesia:dirty_write(NewGist)
    end,

lists:foreach(MigrateFun, Lines).

from_binary(B) when is_binary(B) -> erlang:binary_to_list(B);
from_binary(T) -> T.
