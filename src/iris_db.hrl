
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

-record(history, {
          id,
          messages = []
         }).

-record(history_index, {
          channel_id,
          history_indexes = []
         }).

