-module(iris_channel).

-export([create_channel/2,
         read_channel/1,
         add_user_channel/2]).

-export([on_message_received/2]).

-include("iris_db.hrl").

%% If channel is a process, we don't need to check if it exists, etc.

create_channel(Id, Members) ->
    Now = iris_utils:ts(),
    Channel = #channel{channel_id = Id,
                       members = Members,
                       created_ts = Now,
                       last_ts = Now},
    ok = mnesia:dirty_write(Channel),
    [add_user_channel(User, Id) || User <- Members].

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

on_message_received(User, #{<<user>> := ToUser, <<channel>> := ChannelId} = Message) ->
    case read_channel(ChannelId) of
        [] ->
            create_channel(ChannelId, [User, ToUser]);
        _ ->
            add_user_channel(User, ChannelId),
            add_user_channel(ToUser, ChannelId)
    end,
    ok.

