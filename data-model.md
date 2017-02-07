## Chat data model

### Store messages per user

Each user has a message queue where all his messages are stored. This is the message history. It is independent of the channels, whatever.

```
#message{user, channel_id, text, ts}
#history_chunk{id = {user_id, seq}, start_ts, last_ts, messages}
#history{id = user_id, chunks}
#history_last{id = user_id, last_ts}
```

#### Algorithms

 * __Store new message__. A new message is a message record, it can be stored by appending to the last chunk. If the last chunk is too large, we can create a new chunk by updating the history record and store the message in the chunk.
 * __Get new messages__. It is easy since we only need a ts from where we need to collect all the messages. Then we can group the messages by channel_id, if client application needs that.
 * __Get messages from a channel__. We need to get history chunks one by one, and check if there are messages affected the channel. We can collect messages to a specified ts or we can collect messages until we have a specific number of messages. The later can be risky, since if the channel contains fewer number of messages, we need to scan all the history of the user. Probably in this case we also need to specify a ts.
 * __Maintaining read pointer__. In this model a global read position it is easy to maintain. Read position point to the last message (specified by a timestamp) which was downloaded by the client. The disadvantage is that the client needs to manage if the user really reads the messages per channel. Since once the client application downloaded the messages, we need to move the pointer to the last message (`{user_id, read_ts}`).
 * __Get user channel list__. Without another table, it is very ineffective. Since channel creation, invitation, leave, destroy are not frequent operation, we can apply auxilary tables here. But the least recent channels still can only be computed from the message stream.
 * __Initializing client__. If a new client application (but existing client who has data) initialize their workspace, it needs to get the channel list, and all (or enough) messages from the message stream. Here it cannot be implemented a solution which download the most recent 20 messages per channel.

### Store messages per user and channel

Each user has many channels and messages are stored only in channels. We also have message history but per channel.

```
#message{user, channel_id, text, ts}
#channel{id, members = [user_id, ...], start_ts}
#history_chunk{id = {channel_id, seq}, start_ts, last_ts, messages}
#history{id = channel_id, chunks}
#history_last{id = channel_id, last_ts}

#channel_last{channel_id, last_ts}
#user_channel{user_id, channel_ids}
```

#### Algorithms

 * __Store new message__. Here the message are stored in the history chunk, in the same way before.
 * __Get new messages__. We cannot get all new messages, which created after a specific ts, in one step. We need to scan all channels and needs to check the history if the `last_ts` is more recent than our pointer. It is very ineffective. Probably it is worth creating another table (channel last) which contains the last update ts of a channel.
 * __Get messages from a channel__. It is very easy since we store messages per channel.
 * __Maintaining a read pointer__. It can be used effectively in cooperation with the channel last table only. Unfortunately we don't have possibility to count the messages but when client downloads the new messages, it can count the number of them.
 * __Get user channel list__. Same story here, we need a reverse table here, which the user channel table. With the channel last table the channels can be sorted by activity.
 * __Initializing client__. Here we can do everything which was problem in our previous case.

