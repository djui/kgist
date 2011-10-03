-define(GIST_TABLE, gist).
-define(GIST_KEY_COUNTER, 0).

-record(gist, { id
              , creation_time
              , expires
              , archived
              , language
              , author
              , filename
              , description
              , irc
              , code
              , code_highlighted
              }).
