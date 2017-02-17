-module(iris_channel).
-behaviour(gen_server).

-export([start_link/1,
         create_channel/2,
         leave_channel/2,
         archive_channel/2,
         ensure_channel_proc/1,
         get_channel_proc/1,
         send_message/3,
         send_direct_message/2,
         send_read_status/2,
         read_receipt/2,
         notify_members/1]).

-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

-include("iris_db.hrl").

-record(state, {
          id,
          members
         }).

%% TODO: keep members in the record up-to-date

%%%
%%% API functions
%%%

start_link(Channel) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Channel], []),
    %% TODO: if write fails we will have a orphaned channel process
    ok = mnesia:dirty_write(#channel_proc{channel_id = Channel#channel.id,
                                          pid = Pid}),
    {ok, Pid}.

ensure_channel_proc(ChannelId) ->
    case get_channel_proc(ChannelId) of
        {ok, #channel_proc{pid = Pid}} ->
            {ok, Pid};
        {error, not_found} ->
            case iris_db_channel:read_channel(ChannelId) of
                {error, not_found} ->
                    {error, not_found};
                {ok, Channel} ->
                    iris_channel_sup:start_channel(Channel)
            end
    end.

%% Get the pid of the channel if it has been spawned already.
get_channel_proc(Id) ->
    case mnesia:dirty_read(channel_proc, Id) of
        [] ->
            {error, not_found};
        [ChannelProc] ->
            {ok, ChannelProc}
    end.

%% Create a channel in the database
create_channel(#{channel := Id} = Message, Creator) ->
    create_channel(Message, Id, Creator);
create_channel(Message, Creator) ->
    create_channel(Message, iris_utils:id(), Creator).

leave_channel(Pid, User) ->
    gen_server:call(Pid, {leave, User}).

archive_channel(Pid, User) ->
    gen_server:call(Pid, {archive, User}).

%% Send a message to a channel From a user
send_message(Pid, Message, From) ->
    gen_server:call(Pid, {send, Message, From}).

send_direct_message(Pid, Message) ->
    gen_server:call(Pid, {send_direct, Message}).

send_read_status(Pid, User) ->
    gen_server:call(Pid, {send_read_status, User}).

read_receipt(Pid, Message) ->
    gen_server:call(Pid, {message_read, Message}).

%% Send the channel data to all the members
%% TODO should send via channel proc?
notify_members(#channel{members = Members} = Channel) ->
    broadcast_send(Members, iris_message:channel(Channel)).

%%%
%%% gen_server callbacks
%%%

init([Channel]) ->
    lager:debug("Spawning channel ~p", [Channel]),

    {ok, #state{id = Channel#channel.id,
                members = Channel#channel.members}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({send, Message, From}, _From, State) ->
    lager:debug("Sending message (~p) ~p", [Message, From]),
    %% Create the message
    #{text := Text, ts := TS} = Message,
    Msg = #message{user = From, text = Text, ts = TS},
    ChannelId = State#state.id,
    %% Write it in the history
    iris_db_history:append_message(ChannelId, Msg),
    %% Send back the message stored message
    send_message_stored(From, Message),
    RoutedMessage = Message#{user => From},
    %% Route the message to the receivers
    broadcast_send(From, State#state.members, RoutedMessage),
    {reply, ok, State};

handle_call({send_direct, Message}, _From, State) ->
    case Message of
        #{user := To} ->
            send_to_user(To, Message),
            %% TODO should be not ok if the user is offline
            {reply, ok, State};
        _ ->
            {reply, no_recipient, State}
    end;

handle_call({message_read, Message}, _From, #state{id = Id} = State) ->
    #{from := FromUser, user := ToUser, ts := TS} = Message,

    iris_db_channel:move_read_cursor(Id, FromUser, TS),

    %% Reply to ToUser
    send_to_user(ToUser, Message),

    {reply, ok, State};

handle_call({send_read_status, User}, _From, #state{id = Id} = State) ->
    case iris_db_channel:get_read_cursor(Id) of
        {ok, Cursor} ->
            Ptrs = maps:fold(
                     fun(Usr, TS, Acc) ->
                             [#{user => Usr, ts => TS} | Acc]
                     end, [], Cursor#cursor.read_pointers),

            Reply = #{channel => Id,
                      cursors => Ptrs},

            send_to_user(User, Reply);
        {error, not_found} ->
            ok
    end,
    {reply, ok, State};

handle_call({leave, User}, _From, #state{id = Id} = State) ->
    %% TODO remove the cursor of the user
    case iris_db_channel:leave_channel(Id, User) of
        {ok, Channel} ->
            LeaveMsg = #{type => <<"channel.left">>,
                         channel => Id,
                         user => User},
            State2 = State#state{members = Channel#channel.members},
            broadcast_send(User, State2#state.members, LeaveMsg),
            {reply, ok, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({archive, User}, _From, State) ->
    case iris_db_channel:read_channel(State#state.id) of
        {ok, #channel{owner = User}} ->
            %% If the owner is the User, we can delete
            {reply, ok, State};
        {ok, _} ->
            {reply, {error, only_owner_can_delete}, State};
        _ ->
            {reply, {error, no_such_channel}, State}
    end;

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Internal functions
%%%

create_channel(Message, Id, Creator) ->
    case iris_db_channel:read_channel(Id) of
        {error, not_found} ->
            #{name := Name, invitees := Invitees} = Message,
            Channel = iris_db_channel:insert_channel(Id, Name, Creator, Invitees),
            {ok, Channel};
        {ok, _} ->
            {error, already_exists}
    end.

broadcast_send(Members, Message) ->
    [send_to_user(User, Message) || User <- Members].

broadcast_send(From, Members, Message) ->
    [send_to_user(User, Message) || User <- Members, User =/= From].

send_to_user(User, Message) ->
    case iris_sm:get_session_by_user(User) of
        {ok, #session{pid = Pid} = _Session} ->
            gen_fsm:send_event(Pid, {route, Message});
        {error, not_found} ->
            %% the user is offline
            ok
    end.

send_message_stored(User, Message) ->
    Stored = Message#{user => User,
                      subtype => <<"stored">>},
    Stored2 = maps:remove(text, Stored),
    send_to_user(User, Stored2).
