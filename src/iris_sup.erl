-module(iris_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {#{startegy => one_for_one,
            intensity => 5,
            period => 1000},
          [child(iris_config, []),
           child(iris_db, []),
           child(iris_loader, []),
           start_cowboy()
          ]}
         }.

child(Module, Args) ->
    #{id => Module,
      start => {Module, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => []}.

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/[...]", iris_handler, []},
            {"/ws", iris_ws_handler, []}
        ]}
    ]),
    Args = [
        iris_http_listener,
        5,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ],
    #{id => iris_cowboy,
      start => {cowboy, start_clear, Args},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => []}.
