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
          [#{id => iris_config,
             start => {iris_config, start_link, []},
             restart => permanent,
             shutdown => brutal_kill,
             type => worker,
             modules => []},
           start_cowboy()
          ]}
         }.

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{'_', iris_handler, []}]}
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
