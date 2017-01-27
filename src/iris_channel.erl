-module(iris_channel).
-behaviour(gen_server).

-export([start_link/1,
         create_channel/2,
         get_channel_proc/1,
         send_message/3]).

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

start_link(Id) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Id], []),
    ok = mnesia:dirty_write(#channel_proc{channel_id = Id, pid = Pid}),
    {ok, Pid}.

%% Get the pid of the channel if it has been spawned already.
get_channel_proc(Id) ->
    case mnesia:dirty_read(channel_proc, Id) of
        [] ->
            {error, not_found};
        [ChannelProc] ->
            {ok, ChannelProc}
    end.

%% Create a channel in the database
create_channel(#{<<"channelId">> := Id} = Message, Creator) ->
    create_channel(Message, Id, Creator);
create_channel(Message, Creator) ->
    create_channel(Message, iris_utils:id(), Creator).

%% Send a message to a channel From a user
send_message(Pid, Message, From) ->
    gen_server:call(Pid, {send, Message, From}).

%%%
%%% gen_server callbacks
%%%

init([Id]) ->
    lager:debug("Spawning channel ~p", [Id]),
    {ok, Channel} = read_channel(Id),

    {ok, #state{id = Channel#channel.id,
                members = Channel#channel.members}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({send, Message, From}, _From, State) ->
    lager:debug("Sending message (~p) ~p", [Message, From]),
    #{<<"text">> := Text, <<"ts">> := TS} = Message,
    Msg = #message{
             user = From,
             text = Text,
             ts = TS},
    ChannelId = State#state.id,
    iris_history:append_message(ChannelId, Msg),
    RoutedMessage = Message#{<<"user">> => From},
    broadcast_send(From, State#state.members, RoutedMessage),
    {reply, ok, State};
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
    case read_channel(Id) of
        {error, not_found} ->
            %% TODO implement a message validation layer
            #{<<"invitees">> := Invitees} = Message,
            Channel = insert_channel(Id, [Creator | Invitees]),
            {ok, Channel};
        {ok, _} ->
            {error, already_exists}
    end.

insert_channel(Id, Members) ->
    Now = iris_utils:ts(),
    Channel = #channel{id = Id,
                       members = Members,
                       created_ts = Now,
                       last_ts = Now},
    ok = mnesia:dirty_write(Channel),
    [add_user_channel(User, Id) || User <- Members],
    Channel.

read_channel(Id) ->
    case mnesia:dirty_read(channel, Id) of
        [] ->
            {error, not_found};
        [Channel] ->
            {ok, Channel}
    end.

add_user_channel(User, ChannelId) ->
    case mnesia:dirty_read(user_channel, User) of
        [] ->
            UC = #user_channel{user = User,
                               channel_ids = [ChannelId]},
            mnesia:dirty_write(UC);
        [#user_channel{channel_ids = Ids} = UC] ->
            case lists:member(ChannelId, Ids) of
                false ->
                    UC2 = UC#user_channel{channel_ids = [ChannelId | Ids]},
                    mnesia:dirty_write(UC2);
                true ->
                    ok
            end
    end.

broadcast_send(From, Members, Message) ->
    [send_to_user(User, Message) || User <- Members, User =/= From].

send_to_user(User, Message) ->
    case iris_sm:get_session_by_user(User) of
        {ok, #session{pid = Pid} = _Session} ->
            %% TODO: also move the user's read pointer
            gen_fsm:send_event(Pid, {route, Message});
        {error, not_found} ->
            %% the user is offline
            ok
    end.

