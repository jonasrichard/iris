-module(iris_loader).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, {#{startegy => one_for_one,
            intensity => 5,
            period => 1000},
          modules()
         }
    }.

modules() ->
    Modules = iris_config:get_value(modules, []),
    lists:map(
      fun({Module, Opts}) ->
              #{id => Module,
                start => {Module, start_link, [Opts]},
                restart => permanent,
                shutdown => brutal_kill,
                type => worker,
                modules => []}
      end, Modules).

