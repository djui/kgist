-module(kgist_server).

%%% Exports ====================================================================
-export([ handle_http/1
        ]).

%%% Imports ====================================================================
-import(tulib_calendar, [ unix_ts/0
                        , unix_ts/1
                        , unix_ts_to_datetime/1
                        ]).
-import(tulib_string,   [ fmt/2 ]).

%%% Includes ===================================================================
-include_lib("kgist/include/kgist.hrl").
-include_lib("tulib/include/tulib_calendar.hrl").

%%% Defines ====================================================================
-define(HTML,  [{'Content-Type', "text/html" }]).
-define(PLAIN, [{'Content-Type', "text/plain"}]).

%%% Code =======================================================================
%%% API ------------------------------------------------------------------------
handle_http(Req) ->
  Method = maybe_lift_method(Req), %% Patch
  Rsrc   = Req:resource([lowercase, urldecode]),
  handle(Method, Rsrc, Req).

%%% Dispatches -----------------------------------------------------------------
%% @doc Redirect to new gist form.
handle('GET', [], Req) -> redirect(["gist"], Req);

%% @doc Enter new gist form.
handle('GET', ["gist"], Req) ->
  Recents    = kgist_view:to_list(kgist_db:recents(10)),
  Author     = get_cookie_value("author", "", Req),
  {ok, Lang} = application:get_env(default_language),
  ViewCtx    = [ {page_title,    ""     }
               , {recent_gists,  Recents}
               , {gist_author,   Author }
               , {gist_language, Lang   }
               ],
  Body       = kgist_view:render(gist_new, ViewCtx),
  Req:ok(?HTML, Body);

%% @doc Show all list of all gists.
handle('GET', ["gist", "all"], Req) ->
  Recents = kgist_view:to_list(kgist_db:recents(10)),
  All     = kgist_view:to_list(kgist_db:get_since(0)),
  ViewCtx = [ {page_title,   "All gists"}
            , {recent_gists, Recents    }
            , {gists,        All        }
            ],
  Body    = kgist_view:render(gist_all, ViewCtx),
  Req:ok(?HTML, Body);

%% @doc Show gist.
handle('GET', ["gist", Id], Req) ->
  ContFun = fun(Gist0, CReq) ->
		Gist    = kgist_gist:defaults(Gist0),
                Recents = kgist_view:to_list(kgist_db:recents(10)),
                RelDate = kgist_gist:rel_expiry_date(Gist),
                ViewCtx = [ {page_title,       fmt("Gist ~b", [Gist#gist.id])}
                          , {recent_gists,     Recents                   }
                          , {gist_author,      Gist#gist.author          }
			  , {gist_ctime,       Gist#gist.creation_time   }
                          , {gist_description, Gist#gist.description     }
                          , {gist_expires,     RelDate                   }
                          , {gist_filename,    Gist#gist.filename        }
                          , {gist_hlcode,      Gist#gist.code_highlighted}
                          , {gist_id,          Gist#gist.id              }
                          , {gist_language,    Gist#gist.language        }
                          ],
                Body    = kgist_view:render(gist_view, ViewCtx),
                CReq:ok(?HTML, Body)
    end,
  existance_guard(Id, ContFun, Req);

%% @doc Show gist raw.
handle('GET', ["gist", Id, "raw"], Req) ->
  ContFun = fun(Gist, CReq) ->
                CReq:ok(?PLAIN, Gist#gist.code)
            end,
  existance_guard(Id, ContFun, Req);

%% @doc Download gist.
handle('GET', ["gist", Id, "download"], Req) ->
  ContFun = fun(Gist, CReq) ->
                Header = add_header_download(Gist#gist.filename, ?PLAIN),
                CReq:ok(Header, Gist#gist.code)
            end,
  existance_guard(Id, ContFun, Req);

%% @doc Serve static files.
handle('GET', Path, Req) ->
  Req:file(attachment, file_path(root_dir(), Path));

%% @doc Create new gist.
handle('POST', ["gist"], Req) ->
  handle('PUT', ["gist", kgist_db:next_id()], Req);

%% @doc Create or update gist.
handle('PUT', ["gist", Id], Req) ->
  ContFun = fun(GistId, CReq) ->
                Vals       = Req:parse_post(),
                {ok, Gist} = kgist_gist:sanitize(Vals),
                ok         = kgist_db:put(GistId, Gist),
                CReq:set_cookie("author", Gist#gist.author),
                redirect(["gist", integer_to_list(GistId)], CReq)
            end,
  validate_guard(Id, ContFun, Req);

%% @doc Delete gist.
handle('DELETE', ["gist", Id], Req) ->
  ContFun = fun(Gist, CReq) ->
                ok = kgist_db:archive(Gist),
                redirect([], CReq)
            end,
  existance_guard(Id, ContFun, Req);

%% @doc Nothing matched.
handle(_, _, Req) -> Req:respond(404).

%%% Internals ------------------------------------------------------------------
maybe_lift_method(Req) ->
  case Req:get(method) of
    'POST' ->
      case string:to_upper(proplists:get_value("_method", Req:parse_post(), "")) of
        "DELETE" -> 'DELETE';
        "PUT"    -> 'PUT';
        _        -> 'POST'
      end;
    Method -> Method
  end.

validate_guard(GistId0, ContFun, Req) -> 
  case kgist_gist:convert_id(GistId0) of
    {error, _     } -> Req:respond(400);
    {ok,    GistId} -> ContFun(GistId, Req)
  end.
  
existance_guard(GistId0, ContFun, Req) -> 
  GuardFun = fun(GistId, CReq) ->
                 case kgist_db:get(GistId) of
                   {ok,       Gist    } -> ContFun(Gist, CReq);
                   {expired,  Gist    } -> kgist_db:archive(Gist),
                                           CReq:respond(410);
                   {archived, _       } -> CReq:respond(404);
                   {error,    notfound} -> CReq:respond(404)
                 end
             end,
  validate_guard(GistId0, GuardFun, Req).

add_header(K, V, H) -> [{K,V}|H].

add_header_download(File, H) ->
  add_header('Content-Disposition', fmt("attachment; filename=~s", [File]), H).

get_cookie_value(Key, Def, Req) ->
  Cookies = Req:get_cookies(),
  Cookie  = Req:get_cookie_value(Key, Cookies),
  case Cookie of
    undefined -> Def;
    C         -> C
  end.

file_path(AbsRoot, RelPath0) ->
  RelPath = lists:filter(fun("..") -> false; (_) -> true end, RelPath0),
  filename:join([AbsRoot|RelPath]).

root_dir() ->
  {ok, RootDir} = application:get_env(kgist, root_dir),
  filename:absname(RootDir).

redirect([],   Req) -> Req:respond(303, [{'Location', "/"}], "");
redirect(Path, Req) -> Req:respond(303, [{'Location', filename:join(Path)}], "").
