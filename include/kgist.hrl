-define(GIST_TABLE, gist).

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
