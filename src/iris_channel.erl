-module(iris_channel).
-behaviour(gen_server).

-export([start_link/1,
         create_channel/2,
         get_channel_proc/1,
         send_message/3]).

-export([on_message_received/2]).

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

%%%
%%% API functions
%%%

start_link(Id) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Id], []),
    ok = mnesia:dirty_write(#channel_proc{channel_id = Id, pid = Pid}),
    {ok, Pid}.

get_channel_proc(Id) ->
    case mnesia:dirty_read(channel_proc, Id) of
        [] ->
            {error, not_found};
        [ChannelProc] ->
            {ok, ChannelProc}
    end.

create_channel(#{<<"channelId">> := Id} = Message, Creator) ->
    create_channel(Message, Id, Creator);
create_channel(Message, Creator) ->
    create_channel(Message, iris_utils:id(), Creator).

send_message(Pid, Message, From) ->
    gen_server:call(Pid, {send, Message, From}).

on_message_received(User, #{<<"user">> := ToUser,
                            <<"channel">> := ChannelId} = _Message) ->
    case read_channel(ChannelId) of
        [] ->
            create_channel(ChannelId, [User, ToUser]);
        _ ->
            add_user_channel(User, ChannelId),
            add_user_channel(ToUser, ChannelId)
    end,
    ok.

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
    %% store the message
    %% and notify online members
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
