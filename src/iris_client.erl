-module(iris_client).
-behaviour(gen_fsm).

-export([start_link/1]).

-export([init/1,
         code_change/4,
         terminate/3]).

start_link(SocketPid) ->
    gen_fsm:start_link(?MODULE, [SocketPid], []).

init(SocketPid) ->
    %% TODO: trap exit because websocket is linked with the fsm
    {ok, connected, #{socket => SocketPid}}.

code_change(_OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

connected(Event, StateData) ->
    {next_state, connected, StateData}.

