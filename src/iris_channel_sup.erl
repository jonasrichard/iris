-module(iris_channel_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_channel/2]).

-export([init/1]).

-include("iris_db.hrl").

%%%
%%% API functions
%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_channel(ChannelId, Members) ->
    case iris_channel:get_channel_proc(ChannelId) of
        {ok, #channel_proc{pid = Pid}} ->
            {ok, Pid};
        {error, not_found} ->
            {ok, Pid} = supervisor:start_child(?MODULE, [ChannelId, Members])
    end.

%%%
%%% supervisor callbacks
%%%

init(_Args) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 1000},
         [#{id => iris_channel,
            start => {iris_channel, start_link, []},
            restart => transient,
            shutdown => 1000,
            type => worker,
            modules => []}
         ]}
    }.

