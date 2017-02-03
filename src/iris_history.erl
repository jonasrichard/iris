-module(iris_history).

-export([append_message/2,
         read_messages/1]).

-include("iris_db.hrl").

-define(MAX_MESSAGES, 50).

append_message(ChannelId, Message) ->
    case mnesia:dirty_read(history, ChannelId) of
        [History] ->
            append_or_index(History, Message);
        [] ->
            History = #history{channel_id = ChannelId,
                               messages = [Message]},
            ok = mnesia:dirty_write(History)
    end.

read_messages(ChannelId) ->
    case mnesia:dirty_read(history, ChannelId) of
        [] ->
            [];
        [#history{messages = Messages}] ->
            Messages
    end.

%%%
%%% Internal functions
%%%

%% Append message to history or create an index record
append_or_index(#history{messages = Messages} = History, Message) ->
    %% It is not an index record yet
    case length(Messages) < ?MAX_MESSAGES of
        true ->
            NewHistory = add_to_history(History, Message),
            ok = mnesia:dirty_write(NewHistory);
        false ->
            %% There is no space for a new message, we need to create an index
            %% record.
            #history{channel_id = ChannelId,
                     index = Indices,
                     start_ts = Start,
                     last_ts = Last} = History,
            Index = #history_index{
                       id = {ChannelId, length(Indices) + 1},
                       messages = Messages,
                       start_ts = Start,
                       last_ts = Last},
            Indexed = History#history{
                        messages = [Message],
                        last_ts = Message#message.ts,
                        index = Indices ++ [Index]},
            ok = mnesia:dirty_write(Index),
            ok = mnesia:dirty_write(Indexed)
    end.

add_to_history(History, Message) ->
    #history{channel_id = History#history.channel_id,
             messages = History#history.messages ++ [Message]}.
