-module(iris_client_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_child/2]).

-export([init/1]).

%-include("iris_db.hrl").

%%%
%%% API functions
%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Protocol, Pid) ->
    supervisor:start_child(?MODULE, [Protocol, Pid]).

%%%
%%% supervisor callbacks
%%%

init(_Args) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 1000},
         [#{id => iris_client,
            start => {iris_client, start_link, []},
            restart => temporary,
            shutdown => 1000,  % infinity?
            type => worker,
            modules => []}
         ]}
    }.
