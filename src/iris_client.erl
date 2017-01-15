-module(iris_client).
-behaviour(gen_fsm).

-export([start_link/2,
         connected/2,
         established/2]).

-export([init/1,
         code_change/4,
         terminate/3]).

-record(state, {
          socket,
          format,
          protocol,
          user,
          token
         }).

%%%
%%% API functions
%%%

start_link(websocket, SocketPid) ->
    Arg = {json, websocket, SocketPid},
    gen_fsm:start_link(?MODULE, [Arg], []).

%%%
%%% gen_fsm callbacks
%%%

init([Arg]) ->
    %% TODO: trap exit because websocket is linked with the fsm
    case Arg of
        {json, websocket, Pid} ->
            State = #state{socket = Pid,
                           format = json,
                           protocol = websocket},
            send(#{type => hello}, State),
            {ok, connected, State};
        Other ->
            {stop, "Unknown init state"}
    end.

code_change(_OldVsn, Name, State, _Extra) ->
    {ok, Name, State}.

terminate(_Reason, _Name, _State) ->
    ok.

%%%
%%% State implementation
%%%

connected(Event, State) ->
    case Event of
        #{<<"type">> := <<"auth">>} ->
            #{<<"user">> := User, <<"pass">> := Pass} = Event,
            case iris_hook:run(authenticate, [User, Pass]) of
                {ok, Token} ->
                    State2 = State#state{user = User,
                                         token = Token},
                    send(#{message => <<"Authenticated">>}, State), 
                    {next_state, established, State2};
                {error, _Reason} ->
                    reply(error, [<<"Authentication error">>], State),
                    {next_state, connected, State}
            end;
        _ ->
            {next_state, connected, State}
    end.

established(#{<<"type">> := <<"request">>} = Event, State) ->
    case iris_req:handle(Event) of
        {ok, Result} ->
            reply(response, Result, State),
            {next_state, established, State};
        {error, Reason} ->
            reply(error, Reason, State),
            {next_state, established, State}
    end;            
established(_Event, State) ->
    {next_state, established, State}.

%%%
%%% Internal functions
%%%

send(Msg, #state{format = json,
                 protocol = websocket,
                 socket = WS} = _State) ->
    Json = jsx:encode(Msg),
    WS ! {text, Json}.

reply(error, Args, #state{format = json} = State) ->
    Response =
        case Args of
            [Desc] ->
                iris_msg_json:error(Desc);
            [Desc, Code] ->
                iris_msg_json:error(Desc, Code)
        end,
    send(Response, State).
