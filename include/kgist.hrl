-define(GIST_TABLE, gist).
-define(GIST_KEY_COUNTER, 0).

%% Size included as we can't include the record defintion from dict.erl
-define(is_dict(D), is_record(D, dict, 8)).

-record(gist, { id
              , creation_time
              , expires
              , archived         = false
              , language
              , author
              , filename
              , description
              , irc
              , code
              , code_highlighted
              }).
