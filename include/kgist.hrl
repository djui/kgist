-define(DEFAULT_HOST,    "0.0.0.0").
-define(DEFAULT_PORT,    8001).
-define(DEFAULT_DB_DIR,  "priv/data").
-define(DEFAULT_LOG_DIR, "priv/log").

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
