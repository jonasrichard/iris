## Iris

Run with `RELX_CONFIG_PATH=$PWD/config/sys1.config VMARGS_PATH=$PWD/config/vm1.args _build/default/rel/iris/bin/iris console`.

### Protocol

The protocol is a subset of Slack API.

On connection the client gets a hello message. Probably here we can send the
exact server time (UTC).

```javascript
{
    "type": "hello"
}
```

#### Features

Later a user can choose which features it supports and which features the server knows.

#### Authentication (TBD)

```javascript
{
    "type": "authenticate",
    "user": "username",
    "pass": "p4ssw0rd"
}
```

The reply is the session id. Probably we need to support resources here.

```javascript
{
    "type": "authenticated",
    "sessionId": "22FE23BC34"
}
```

#### Create channel

One can create a channel in order that he could send messages to multiple
persons (or just one).

```javascript
{
    "type": "channel.create",
    "name": "optional name of the channel",
    "channelId": "optional id",
    "invitees": [
        "user1",
        "user2"
    ]
}
```

One type of the replies is for the creator

```javascript
{
    "type": "channel.created",
    "name": "name",
    "channelId": "id",
    "members": ["user1", "user2"]
}
```

And the invited parties get

```javascript
{
	"type": "channel.invited",
    "name": "name",
    "channelId": "id",
    "members", ["user1", "user2"]	
}
```


#### Get channel list

Get the list of all channels the user has been invited.

```javascript
{
    "type": "channel.list"
}
```

The result is the list of channel data

```javascript
{
    "type": "channel.list",
    "channels": [
    {
    	"id": "channel id",
        "name": "channel name",
        "members": [],
        "created_ts": "timestamp",
        "last_ts": "timestamp"
    }
    ]
}
```

#### Leave channel, archive channel

#### Message sending

Messages can be sent through a channel. Channel has an id and has members.
If it is a point-to-point message generally two parties are here and there
may be bots which can send extra messages (moderation, adivces, news, etc.).

```javascript
{
    "type": "message",
    "subtype": "send",
    "from": "originator",
    "channel": "channel id",
    "ts": "1432132987.001278",
    "text": "This is the body of the message"
}
```

This will send the message to all resources to another user.

```javascript
{
    "type": "message",
    "subtype": "incoming",
    "from": "originator",
    "channel": "channel id",
    "ts": "1432132987.001278",
    "text": "This is the body of the message"
}
```

#### Get channel history

```javascript
{
    "type": "channel.read",
    "channelId": "id",
    "start_ts": "start timestamp",
    "end_ts": "option end timestamp"
}
```

In this case a read receipt should be sent to the users who authored messages.

#### Read receipt

Client needs to send that it received the messages.

```
 sender         server          receiver
   |               |               | 
   |     send      |               |
   |-------------->|               | 
   |               |               | 
   |     stored    |               | 
   |<--------------|    incoming   | 
   |               |-------------->| 
   |               |               | 
   |               |    received   | 
   |    received   |<--------------| 
   |<--------------|               | 
   |               |     read      | * client read
   |     read      |<--------------| 
   |<--------------|               | 
   |               |               | 

```

The first message is sent by the sender.

```javascript
{
    "type": "message",
    "subtype": "send",
    "channel": "channel id"
    "from": "originator"
    "ts" : "timestamp",
    "text": "message text"
}
```

Server sends back a stored message once it stored the message persistently. Until
this message clients cannot be sure that the message stored successfully.

```javascript
{
    "type": "message",
    "subtype": "stored",
    "channel": "channel id",
    "ts": "timestamp of last message"
}
```

Also other parties in the channel will get the message as incoming message.

```javascript
{
    "type": "message",
    "subtype": "incoming",
    "channel": "channel id",
    "from": "originator/sender",
    "text": "Text of the message",
    "ts": "timestamp of last message"
}
```

Client application should sign that it has received the message.

```javascript
{
    "type": "message",
    "subtype": "received",
    "channel": "channel id",
    "from": "receiver",
    "to": "sender",
    "ts": "timestamp of last message"
}
```

This message will be sent only the sender of the message.

The same story is played with `read` subtype message.

It means that it gets all the messages by the last message.

* user: id -> name, pass, email
* channel: id -> name, members, created\_ts, last\_ts
* history: channel\_id -> messages
* cursor: user\_resource\_channel -> ts
* unread: user\_resource\_channel -> count


