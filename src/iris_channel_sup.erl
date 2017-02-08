-module(iris_channel_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_channel/1]).

-export([init/1]).

-include("iris_db.hrl").

%%%
%%% API functions
%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_channel(Channel) ->
    supervisor:start_child(?MODULE, [Channel]).

%%%
%%% supervisor callbacks
%%%

init(_Args) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 1000},
         [#{id => iris_channel,
            start => {iris_channel, start_link, []},
            restart => temporary,
            shutdown => 1000,  % infinity?
            type => worker,
            modules => []}
         ]}
    }.

