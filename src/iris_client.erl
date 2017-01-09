-module(iris_client).
-behaviour(gen_fsm).

-export([start_link/1,
         connected/2]).

-export([init/1,
         code_change/4,
         terminate/3]).

%%%
%%% API functions
%%%

start_link(SocketPid) ->
    gen_fsm:start_link(?MODULE, [SocketPid], []).

%%%
%%% gen_fsm callbacks
%%%

init([SocketPid]) ->
    %% TODO: trap exit because websocket is linked with the fsm
    StateData = #{socket => SocketPid},
    send(StateData, #{type => hello}),
    {ok, connected, StateData}.

code_change(_OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

%%%
%%% State implementation
%%%

connected(Event, StateData) ->
    {next_state, connected, StateData}.

%%%
%%% Internal functions
%%%

send(#{socket := WS} = StateData, Msg) ->
    %% TODO: generic conversion, and generic send api
    %% hide the fact that socket is websocket
    Json = jsx:encode(Msg),
    WS ! {text, Json}.
