
-record(session, {
          id,
          pid,
          user
         }).

-record(channel, {
          channel_id,
          members = [],
          created_ts,
          last_ts
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

