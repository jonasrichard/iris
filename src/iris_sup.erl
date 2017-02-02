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
           start_cowboy()
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

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/[...]", iris_handler, []},
            {"/ws", iris_ws_handler, #{}},
            {"/[...]", cowboy_static, {priv_dir, iris, "html"}}
        ]}
    ]),
    Args = [
        iris_http_listener,
        5,
        [{port, get_http_port()}],
        #{env => #{dispatch => Dispatch}}
    ],
    #{id => iris_cowboy,
      start => {cowboy, start_clear, Args},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => []}.

get_http_port() ->
    case application:get_env(iris, http) of
        {ok, Keys} ->
            proplists:get_value(port, Keys, 8080);
        undefined ->
            8080
    end.

