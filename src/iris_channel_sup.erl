-module(iris_channel_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_channel/0]).

-export([init/1]).

%%%
%%% API functions
%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_channel(ChannelId) ->
    case mnesia:dirty_read(channel_proc, ChannelId) of
        [] ->
            %% we can create the channel and store
            ok;
        [Channel] ->
            %% we need to pass the pid back if
            %% the process is alive. And this is the hard part :)
            ok
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

