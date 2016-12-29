-module(iris_loader).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    %% !!! TOO EARLY
    modules(),
    {ok, {#{startegy => one_for_one,
            intensity => 5,
            period => 1000},
          []}}.

modules() ->
    Modules = iris_config:get_value(modules, []),
    %% Call init/1 of modules and also start supervised
    %% child processes if the modules needs them
    lists:foreach(
      fun({Module, Opts}) ->
              lager:info("Initializing modules ~p", [Module]),
              Module:init(Opts)
      end, Modules).

