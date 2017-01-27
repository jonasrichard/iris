
%% Sessions are stored in memory. Id is a generated id and pid is the
%% iris_client pid which processes the incoming messages.
-record(session, {
          id,
          pid,
          user
         }).

-record(channel, {
          id,
          members = [],
          created_ts,
          last_ts
         }).

-record(channel_proc, {
          channel_id,
          pid
         }).

-record(user_channel, {
          user,
          channel_ids = []
         }).

%% It is not a table, it is put in the channel history
-record(message, {
          user,
          text,
          ts
         }).

%% Simplified version of chat history, there is no chunking yet.
-record(history, {
          channel_id,
          messages = []
         }).

%% Index of the history of a channel.
-record(history_index, {
          channel_id,
          history_indexes = []
         }).

