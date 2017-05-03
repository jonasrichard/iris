-module(iris_app).
-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    %%application:start(lager),
    metrics(),
    case os:getenv("OTHER_NODE") of
        Empty when Empty =:= false orelse Empty =:= "" ->
            wait_for_mnesia();
        "NO" ->
            ok;
        NodeEnv ->
            Node = list_to_atom(NodeEnv),
            iris_mnesia:join(Node)
    end,
    OkPid =iris_sup:start_link(),
    start_cowboy(),
    OkPid.

stop(_State) ->
    cowboy:stop_listener(iris_http_listener),
    mnesia:stop(),
    ok.

%%%
%%% Internal functions
%%%

wait_for_mnesia() ->
    lager:info("Starting mnesia..."),
    iris_mnesia:ensure_schema(),
    iris_mnesia:ensure_tables(),
    lager:info("Waiting for mnesia tables..."),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
    lager:info("Tables are ready").

metrics() ->
    exometer:new("session.count", gauge).
    %% clients should be registered under a supervisor and this metric will be used
    %% by a subscription

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/[...]", iris_handler, []},
            {"/ws", iris_ws_handler, #{}},
            {"/[...]", cowboy_static, {priv_dir, iris, "html"}}
        ]}
    ]),
    cowboy:start_clear(iris_http_listener, 5,
                       [{port, get_http_port()}],
                       #{env => #{dispatch => Dispatch}}).

get_http_port() ->
    case application:get_env(iris, http) of
        {ok, Keys} ->
            proplists:get_value(port, Keys, 8080);
        undefined ->
            8080
    end.

