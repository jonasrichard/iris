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
          [child(iris_hook, []),
           child(iris_config, []),
           child(iris_req, []),
           child(iris_db, []),
           child(iris_loader, []),
           child_sup(iris_channel_sup, []),
           child_sup(iris_client_sup, []),
           child(iris_metrics, [])
          ]}
         }.

child(Module, Args) ->
    #{id => Module,
      start => {Module, start_link, Args},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => []}.

child_sup(Module, Args) ->
    Spec = child(Module, Args),
    Spec#{shutdown => infinity,
          type => supervisor}.
