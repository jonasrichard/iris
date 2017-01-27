-module(iris_history).

-export([append_message/2]).

-include("iris_db.hrl").

append_message(ChannelId, Message) ->
    case mnesia:dirty_read(history, ChannelId) of
        [History] ->
            NewHistory = add_to_history(History, Message),
            ok = mnesia:dirty_write(NewHistory);
        [] ->
            History = #history{channel_id = ChannelId,
                               messages = [Message]},
            ok = mnesia:dirty_write(History)
    end.

add_to_history(History, Message) ->
    #history{channel_id = History#history.channel_id,
             messages = History#history.messages ++ [Message]}.
