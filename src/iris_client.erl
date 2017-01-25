-module(iris_client).
-behaviour(gen_fsm).

-export([start_link/2,
         connected/2,
         established/2]).

-export([init/1,
         handle_info/3,
         code_change/4,
         terminate/3]).

-compile({nowarn_unused_function, [{connected, 2},
                                   {established, 2}]}).

-include("iris_db.hrl").

-record(state, {
          socket,                   %% pid of the sender/receiver
          protocol,                 %% {json, websocket} | raw
          user,
          token,                    %% Token for client application
          sid,                      %% session id
          channels = #{}            %% map of channels (id, pid)
         }).

%% Message macros
-define(CHANNEL, <<"channel">>).
-define(TYPE, <<"type">>).
-define(USER, <<"user">>).

%% TODO monitor channels

%%%
%%% API functions
%%%

start_link(websocket, SocketPid) ->
    gen_fsm:start_link(?MODULE, [{json, websocket, SocketPid}], []);
start_link(raw, Pid) ->
    gen_fsm:start_link(?MODULE, [{raw, Pid}], []).

%%%
%%% gen_fsm callbacks
%%%

init([Arg]) ->
    %% TODO: trap exit because websocket is linked with the fsm
    case Arg of
        {json, websocket, Pid} ->
            State = #state{socket = Pid, protocol = {json, websocket}},
            erlang:monitor(process, Pid),
            send(iris_message:hello(), State),
            {ok, connected, State};
        {raw, Pid} ->
            State = #state{socket = Pid, protocol = raw},
            erlang:monitor(process, Pid),
            send(iris_message:hello(), State),
            {ok, connected, State};
        _Other ->
            lager:error("Unknown init args: ~p", [Arg]),
            {stop, unknown_init_args}
    end.

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, _Name,
            #state{socket = _Pid} = State) ->
    {stop, normal, State};
handle_info(Info, Name, State) ->
    lager:info("~p", [Info]),
    {next_state, Name, State}.

code_change(_OldVsn, Name, State, _Extra) ->
    {ok, Name, State}.

terminate(_Reason, _Name, #state{sid = SessionId} = _State) ->
    iris_sm:delete_session(SessionId), 
    ok.

%%%
%%% State implementation
%%%

connected(#{?TYPE := <<"auth">>} = Event, State) ->
    #{<<"user">> := User, <<"pass">> := Pass} = Event,
    case iris_hook:run(authenticate, [User, Pass]) of
        ok ->
            %% no registered hook could generate a token
            reply(error, [<<"Authentication error">>], State),
            {next_state, connected, State};
        {ok, Token} ->
            SessionId = iris_sm:save_session(self(), User),
            iris_hook:run(session_created, [User, SessionId]),
            State2 = State#state{user = User, token = Token, sid = SessionId},
            send(iris_message:session(SessionId), State),
            {next_state, established, State2};
        {error, _Reason} ->
            reply(error, [<<"Authentication error">>], State),
            {next_state, connected, State}
    end;
connected(_Event, State) ->
    {next_state, connected, State}.

established(#{?TYPE := <<"message">>, ?USER := ToUser} = Event,
            #state{user = User} = State) ->
    case iris_hook:run(message_received, [User, Event]) of
        drop ->
            %% we drop the message
            {next_state, established, State};
        _ ->
            case send_to_channel(Event, User, ToUser, State) of
                {ok, NewState} ->
                    {next_state, established, NewState};
                {Reason, NewState} ->
                    {stop, Reason, NewState}
            end
    end;
established(#{?TYPE := <<"request">>} = Event, State) ->
    case iris_req:handle(Event) of
        {ok, Result} ->
            reply(response, Result, State),
            {next_state, established, State};
        {error, Reason} ->
            reply(error, Reason, State),
            {next_state, established, State}
    end;
established({route, Event}, State) ->
    send(Event, State),
    {next_state, established, State};
established(_Event, State) ->
    {next_state, established, State}.

%%%
%%% Internal functions
%%%

%route_message(ClientPid, User, Event) ->
%    Response = Event#{<<"user">> => User},
%    lager:info("Send to ~p ~p", [ClientPid, Response]),
%    gen_fsm:send_event(ClientPid, {route, Response}).

send_to_channel(Message, From, To, #state{channels = Channels} = State) ->
    #{?CHANNEL := ChannelId} = Message,
    case Channels of
        #{ChannelId := Pid} ->
            %% TODO message sent to channel hook?
            Result = iris_channel:send_message(Pid, Message, From, To),
            {Result, State};
        _ ->
            case iris_channel:get_channel_proc(ChannelId) of
                {ok, Pid} ->
                    %% The channel is started already, store its pid
                    NewChannels = Channels#{ChannelId => Pid},
                    Result = iris_channel:send_message(Pid, Message, From, To),
                    {Result, State#state{channels = NewChannels}};
                {error, not_found} ->
                    {ok, Pid} = iris_channel_sup:start_channel(ChannelId, [From, To]),
                    Result = iris_channel:send_message(Pid, Message, From, To),
                    {Result, State}
            end
    end.

send(Msg, #state{protocol = {json, websocket}, socket = WS} = _State) ->
    Json = jsx:encode(Msg),
    WS ! {text, Json};
send(Msg, #state{protocol = raw, socket = Pid}) ->
    Pid ! {reply, Msg}.

reply(error, Args, #state{protocol = {json, _}} = State) ->
    Response =
        case Args of
            [Desc] ->
                iris_msg_json:error_msg(Desc);
            [Desc, Code] ->
                iris_msg_json:error_msg(Desc, Code)
        end,
    send(Response, State).
