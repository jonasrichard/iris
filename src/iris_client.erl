-module(iris_client).
-behaviour(gen_fsm).

-export([start_link/2,
         connected/2,
         established/2]).

-export([init/1,
         code_change/4,
         terminate/3]).

%%%
%%% API functions
%%%

start_link(websocket, SocketPid) ->
    Arg = #{format => json,
            protocol => websocket,
            socket => SocketPid},
    gen_fsm:start_link(?MODULE, [Arg], []).

%%%
%%% gen_fsm callbacks
%%%

init([InitState]) ->
    %% TODO: trap exit because websocket is linked with the fsm
    send(#{type => hello}, InitState),
    {ok, connected, InitState}.

code_change(_OldVsn, Name, State, _Extra) ->
    {ok, Name, State}.

terminate(_Reason, _Name, _State) ->
    ok.

%%%
%%% State implementation
%%%

connected(Event, State) ->
    case Event of
        #{type := auth} ->
            #{user := User, pass := Pass} = Event,
            case iris_hook:run(authenticate, [User, Pass]) of
                {ok, Token} ->
                    State2 = State#{
                               user => User,
                               token => Token},
                    {next_state, established, State2};
                {error, _Reason} ->
                    reply(error, [<<"Authentication error">>], State),
                    {next_state, established, State}
            end;
        _ ->
            {next_state, connected, State}
    end.

established(_Event, State) ->
    {next_state, established, State}.

%%%
%%% Internal functions
%%%

send(Msg, #{protocol := websocket, socket := WS} = _State) ->
    Json = jsx:encode(Msg),
    WS ! {text, Json}.

reply(error, Args, #{format := json} = State) ->
    Response =
        case Args of
            [Desc] ->
                iris_msg_json:error(Desc);
            [Desc, Code] ->
                iris_msg_json:error(Desc, Code)
        end,
    send(Response, State).
