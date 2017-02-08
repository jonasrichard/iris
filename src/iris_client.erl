-module(iris_client).
-behaviour(gen_fsm).

-export([start_link/2,
         connected/2,
         established/2]).

-export([init/1,
         handle_info/3,
         handle_event/3,
         handle_sync_event/4,
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
handle_info(kick_out, _Name, State) ->
    lager:info("User ~p has been kicked out", [State#state.user]),
    {stop, normal, State};
handle_info(Info, Name, State) ->
    lager:info("~p", [Info]),
    {next_state, Name, State}.

handle_event(_Event, Name, State) ->
    {next_state, Name, State}.

handle_sync_event(_Event, _From, Name, State) ->
    {reply, ok, Name, State}.

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
        {ok, false} ->
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

established(#{?TYPE := <<"message">>} = Event, #state{user = User} = State) ->
    case Event of
        #{<<"subtype">> := <<"ack">>} ->
            %% user has read the message
            send_receipt(Event, State),
            {next_state, established, State};
        _ ->
            %% no subtype, it is a new message
            Event2 = ensure_ts(Event),
            case iris_hook:run(message_received, [User, Event2]) of
                drop ->
                    %% we drop the message
                    {next_state, established, State};
                _ ->
                    case send_to_channel(Event2, User, State) of
                        {ok, NewState} ->
                            {next_state, established, NewState};
                        {_Reason, NewState} ->
                            reply(error, [<<"No such channel">>], NewState),
                            {next_state, established, NewState}
                    end
            end
    end;

established(#{?TYPE := <<"channel.create">>} = Event,
            #state{user = User} = State) ->
    case iris_channel:create_channel(Event, User) of
        {ok, Channel} ->
            iris_channel:notify_members(Channel),
            {next_state, established, State};
        {error, _Reason} ->
            reply(error, [<<"Error during creating channel">>], State),
            {next_state, established, State}
    end;

established(#{?TYPE := <<"channel.list">>} = _Event,
            #state{user = User} = State) ->
    ChannelIds = iris_channel:read_user_channel(User),
    lists:foreach(
      fun(ChannelId) ->
              case iris_channel:read_channel(ChannelId) of
                  {error, not_found} ->
                      ok;
                  {ok, Channel} ->
                      send(#{?TYPE => <<"channel.get">>,
                             <<"channelId">> => ChannelId,
                             <<"channelName">> => Channel#channel.name},
                           State)
              end
      end,
      ChannelIds),
    {next_state, established, State};

established(#{?TYPE := <<"channel.history">>, ?CHANNEL := ChannelId}, State) ->
    Msgs= iris_history:read_messages(ChannelId),
    Reply = #{<<"type">> => <<"channel.history">>,
              <<"messages">> => [
                #{<<"type">> => <<"message">>,
                  <<"user">> => Msg#message.user,
                  <<"text">> => Msg#message.text,
                  <<"ts">> => Msg#message.ts} || Msg <- Msgs]},
    send(Reply, State),
    {next_state, established, State};

established(#{?TYPE := <<"request">>} = Event, State) ->
    case iris_req:handle(Event) of
        {ok, Result} ->
            reply(response, Result, State),
            {next_state, established, State};
        {error, Reason} ->
            reply(error, Reason, State),
            {next_state, established, State}
    end;

established({route, Message}, State) ->
    send(Message, State),
    {next_state, established, State};

established(_Event, State) ->
    {next_state, established, State}.

%%%
%%% Internal functions
%%%

%% TODO: monitor the channel process if we store it
send_to_channel(Message, From, #state{channels = Channels} = State) ->
    #{?CHANNEL := ChannelId} = Message,
    case Channels of
        #{ChannelId := Pid} ->
            %% TODO send message to channel hook?
            Result = iris_channel:send_message(Pid, Message, From),
            {Result, State};
        _ ->
            case iris_channel:ensure_channel_proc(ChannelId) of
                {ok, Pid} ->
                    %% The channel is started already, store its pid
                    NewChannels = Channels#{ChannelId => Pid},
                    Result = iris_channel:send_message(Pid, Message, From),
                    {Result, State#state{channels = NewChannels}};
                Other ->
                    lager:error("Send error: ~p", [Other]),
                    {error, State}
            end
    end.

send_receipt(Message, #state{channels = Channels} = State) ->
    Rcpt = read_ack(Message),
    #{?CHANNEL := ChannelId} = Message,
    case Channels of
        #{ChannelId := Pid} ->
            %% TODO send message to channel hook?
            Result = iris_channel:send_direct_message(Pid, Rcpt),
            {Result, State};
        _ ->
            case iris_channel:ensure_channel_proc(ChannelId) of
                {ok, Pid} ->
                    %% The channel is started already, store its pid
                    NewChannels = Channels#{ChannelId => Pid},
                    Result = iris_channel:send_direct_message(Pid, Rcpt),
                    {Result, State#state{channels = NewChannels}};
                Other ->
                    lager:error("Send error: ~p", [Other]),
                    {error, State}
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

%% Ensure that message has timestamp
ensure_ts(#{<<"ts">> := _} = Message) ->
    Message;
ensure_ts(Message) ->
    Message#{<<"ts">> => iris_utils:ts()}.

read_ack(Message) ->
    #{<<"type">> => <<"message">>,
      <<"subtype">> => <<"read">>,
      <<"user">> => maps:get(<<"user">>, Message),
      <<"reader">> => maps:get(<<"reader">>, Message),
      <<"channel">> => maps:get(<<"channel">>, Message),
      <<"ts">> => maps:get(<<"ts">>, Message)}.

