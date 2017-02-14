
%% Sessions are stored in memory. Id is a generated id and pid is the
%% iris_client pid which processes the incoming messages.
-record(session, {
          id,
          pid,
          user
         }).

-record(channel, {
          id,
          name,
          owner,
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

%% History record is a generic one, it supports small chat history
%% and chunked. We need to implement it with the same structure
%% since some backends (mnesia) cannot allow changing record structure.
%%
%% Fields:
%%   - id (channel_id or {channel_id, seq})
%%   - messages (it is a list if it is not an index record)
%%   - index
%%     - empty if it is not an index record
%%     - list of history_idx records
-record(history, {
          channel_id,
          start_ts,
          last_ts,
          messages = [],
          index = []
         }).

-record(history_index, {
          id,       %% {channel_id, seq}
          messages,
          start_ts,
          last_ts
         }).
