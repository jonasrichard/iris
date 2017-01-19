-module(iris_app).
-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    application:start(lager),
    wait_for_mnesia(),
    iris_sup:start_link().

stop(_State) ->
    mnesia:stop(),
    ok.

%%%
%%% Internal functions
%%%

wait_for_mnesia() ->
    lager:info("Starting mnesia..."),
    iris_mnesia:ensure_schema(),
    lager:info("Waiting for mnesia tables..."),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
    lager:info("Tables are ready").

