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

handle_info({'DOWN', _Ref, process, Pid, _Reason}, _Name,
            #state{socket = Pid} = State) ->
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

established(Event, State) when is_map(Event) ->
    try iris_message:parse(Event) of
        #{type := <<"message">>, subtype := <<"send">>} = Message ->
            %% user sent a message
            do_handle_user_sent_message(Message, State);

        #{type := <<"message">>, subtype := <<"sent">>} = MsgAck ->
            do_handle_ack_message(MsgAck, State);

        #{type := <<"message">>, subtype := <<"read">>} = MsgRead ->
            do_handle_message_read(MsgRead, State);

        #{type := <<"channel.create">>} = CrtChannel ->
            do_handle_create_channel(CrtChannel, State);

        #{type := <<"channel.list">>} ->
            do_handle_list_channels(State);

        #{type := <<"channel.history">>} = History ->
            do_handle_history(History, State);

        #{type := <<"channel.status">>} = Status ->
            do_handle_channel_status(Status, State);

        #{type := <<"channel.archive">>} = Archive ->
            do_handle_archive_channel(Archive, State);

        #{type := <<"channel.leave">>} = Leave ->
            do_handle_leave_channel(Leave, State)

    catch
        Type:Reason ->
            lager:error("Message parsing error: ~p ~p ~p",
                        [Type, Reason, Event]),
            reply(error, [<<"Invalid message">>], State),
            {next_state, established, State}
    end;

established({route, Message}, State) ->
    send(Message, State),
    {next_state, established, State};

established(Event, State) ->
    lager:warning("Unhandler event ~p", [Event]),
    {next_state, established, State}.

%%%
%%% Internal functions
%%%

do_handle_user_sent_message(Message, #state{user = User} = State) ->
    Message2 = ensure_ts(Message),
    case iris_hook:run(message_received, [User, Message2]) of
        drop ->
            %% we drop the message
            {next_state, established, State};
        _ ->
            case send_to_channel(Message2, User, State) of
                {ok, NewState} ->
                    {next_state, established, NewState};
                {_Reason, NewState} ->
                    reply(error, [<<"No such channel">>], NewState),
                    {next_state, established, NewState}
            end
    end.

do_handle_ack_message(Ack, State) ->
    {_, State2} = send_direct_message(read_ack(Ack), State),
    {next_state, established, State2}.

do_handle_create_channel(CrtChannel, #state{user = User} = State) ->
    case iris_channel:create_channel(CrtChannel, User) of
        {ok, Channel} ->
            iris_channel:notify_members(Channel),
            {next_state, established, State};
        {error, _Reason} ->
            reply(error, [<<"Error during creating channel">>], State),
            {next_state, established, State}
    end.

do_handle_list_channels(#state{user = User} = State) ->
    ChannelIds = iris_db_channel:read_user_channel(User),
    sets:fold(
      fun(ChannelId, _Acc) ->
              case iris_db_channel:read_channel(ChannelId) of
                  {error, not_found} ->
                      ok;
                  {ok, Channel} ->
                      send(#{type => <<"channel.get">>,
                             id => ChannelId,
                             name => Channel#channel.name,
                             owner => Channel#channel.owner},
                           State)
              end
      end, undefined, ChannelIds),
    {next_state, established, State}.


do_handle_channel_status(#{channel := ChannelId} = _Status,
                         #state{user = User} = State) ->
    case get_channel_pid(ChannelId, State) of
        {ok, Pid, State2} ->
            iris_channel:send_read_status(Pid, User),
            {next_state, established, State2};
        {error, State2} ->
            reply(error, [<<"Channel cannot be found">>], State2),
            {next_state, established, State2}
    end.

do_handle_archive_channel(#{channel := ChannelId} = _Archive,
                          #state{user = User} = State) ->
    case get_channel_pid(ChannelId, State) of
        {ok, Pid, State2} ->
            case iris_channel:archive_channel(Pid, User) of
                ok ->
                    Channels2 = maps:remove(ChannelId, State#state.channels),
                    {next_state, established, State2#state{channels = Channels2}};
                {error, _Reason} ->
                    reply(error, [<<"Channel cannot be left">>], State2),
                    {next_state, established, State2}
            end;
        {error, State2} ->
            reply(error, [<<"Channel cannot be found">>], State2),
            {next_state, established, State2}
    end.

do_handle_leave_channel(#{channel := ChannelId} = _Leave,
                        #state{user = User} = State) ->
    case get_channel_pid(ChannelId, State) of
        {ok, Pid, State2} ->
            case iris_channel:leave_channel(Pid, User) of
                ok ->
                    Channels2 = maps:remove(ChannelId, State#state.channels),
                    {next_state, established, State2#state{channels = Channels2}};
                {error, _Reason} ->
                    reply(error, [<<"Channel cannot be left">>], State2),
                    {next_state, established, State2}
            end;
        {error, State2} ->
            reply(error, [<<"Channel cannot be found">>], State2),
            {next_state, established, State2}
    end.

do_handle_history(#{channel := ChannelId}, State) ->
    Msgs= iris_db_history:read_messages(ChannelId),
    Reply = #{type => <<"channel.history">>,
              messages => [
                #{type => <<"message">>,
                  user => Msg#message.user,
                  text => Msg#message.text,
                  ts => Msg#message.ts} || Msg <- Msgs]},
    send(Reply, State),
    {next_state, established, State}.

do_handle_message_read(#{channel := ChannelId} = MsgRead,
                       #state{user = User} = State) ->
    Reply = MsgRead#{from => User},
    Reply2 = maps:remove(user, Reply),
    %% TODO error?
    case get_channel_pid(ChannelId, State) of
        {ok, Pid, NewState} ->
            _Result = iris_channel:read_receipt(Pid, Reply2),
            {next_state, established, NewState};
        {error, NewState} ->
            %% TODO: send error
            {next_state, established, NewState}
    end.

%% TODO: monitor the channel process if we store it
send_to_channel(#{channel := ChannelId} = Message, From, State) ->
    case get_channel_pid(ChannelId, State) of
        {ok, Pid, NewState} ->
            Result = iris_channel:send_message(Pid, Message, From),
            {Result, NewState};
        {error, NewState} ->
            %% TODO: send error
            {error, NewState}
    end.

send_direct_message(#{channel := ChannelId} = Message, State) ->
    case get_channel_pid(ChannelId, State) of
        {ok, Pid, NewState} ->
            Result = iris_channel:send_direct_message(Pid, Message),
            {Result, NewState};
        {error, NewState} ->
            %% TODO: send error
            {error, NewState}
    end.

get_channel_pid(ChannelId, #state{channels = Channels} = State) ->
    case Channels of
        #{ChannelId := Pid} ->
            {ok, Pid, State};
        _ ->
            case iris_channel:ensure_channel_proc(ChannelId) of
                {ok, Pid} ->
                    %% The channel is started already, store its pid
                    NewChannels = Channels#{ChannelId => Pid},
                    {ok, Pid, State#state{channels = NewChannels}};
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
ensure_ts(#{ts := _} = Message) ->
    Message;
ensure_ts(Message) ->
    Message#{ts => iris_utils:ts()}.

read_ack(Message) ->
    #{type => <<"message">>,
      subtype => <<"read">>,
      user => maps:get(user, Message),
      reader => maps:get(reader, Message),
      channel => maps:get(channel, Message),
      ts => maps:get(ts, Message)}.

