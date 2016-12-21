-module(iris_app).
-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    iris_sup:start_link().

stop(_State) ->
    ok.
