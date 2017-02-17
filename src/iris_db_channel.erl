-module(iris_db_channel).

-export([read_channel/1,
         read_user_channel/1,
         insert_channel/4,
         leave_channel/2,
         add_user_channel/2]).

-export([move_read_cursor/3]).

-include("iris_db.hrl").

read_channel(Id) ->
    case mnesia:dirty_read(channel, Id) of
        [] ->
            {error, not_found};
        [Channel] ->
            {ok, Channel}
    end.

read_user_channel(User) ->
    case mnesia:dirty_read(user_channel, User) of
        [] ->
            [];
        [#user_channel{channel_ids = Ids}] ->
            Ids
    end.

insert_channel(Id, Name, Owner, Members) ->
    Now = iris_utils:ts(),
    AllMembers = [Owner | Members],
    Channel = #channel{id = Id,
                       name = Name,
                       owner = Owner,
                       members = AllMembers,
                       created_ts = Now,
                       last_ts = Now},
    ok = mnesia:dirty_write(Channel),
    [add_user_channel(User, Id) || User <- AllMembers],
    Channel.

leave_channel(Id, User) ->
    case read_channel(Id) of
        {ok, #channel{members = M} = Channel} ->
            M2 = lists:delete(User, M),
            Channel2 = Channel#channel{members = M2},
            ok = mnesia:dirty_write(Channel2),
            %% TODO update user_channels
            {ok, Channel2};
        Error ->
            Error
    end.

add_user_channel(User, ChannelId) ->
    UC =
        case mnesia:dirty_read(user_channel, User) of
            [] ->
                #user_channel{user = User};
            [Record] ->
                Record
        end,

    #user_channel{channel_ids = Channels} = UC,
    UC2 = UC#user_channel{channel_ids = sets:add_element(ChannelId, Channels)},
    ok = mnesia:dirty_write(UC2),
    {ok, UC2}.

move_read_cursor(ChannelId, UserId, Ts) ->
    Cursor =
        case mnesia:dirty_read(cursor, ChannelId) of
            [C] ->
                C;
            [] ->
                #cursor{channel_id = ChannelId}
        end,

    Pointers2 = maps:put(UserId, Ts, Cursor#cursor.read_pointers),
    Cursor2 = Cursor#cursor{read_pointers = Pointers2},
    ok = mnesia:dirty_write(Cursor2),
    {ok, Cursor2}.
