
## Iris

### Protocol

The protocol is a subset of Slack API.

On connection the client gets a hello message. Probably here we can send the
exact server time (UTC).

```
{
    "type": "hello"
}
```

#### Features

In the future it would be good if some users won't get offline messages when
they come online, etc.

#### Authentication (TBD)

```
{
    "type": "authenticate",
    "user": "username",
    "pass": "p4ssw0rd"
}
```

The reply is the session id. Probably we need to support resources here.

```
{
    "type": "authenticated",
    "session": "22FE23BC34"
}
```

#### Message sending

Messages can be sent through a channel. Channel has an id and has members.
If it is a point-to-point message generally two parties are here and there
may be bots which can send extra messages (moderation, adivces, news, etc.).

```
{
    "type": "message",
    "subtype": "send",
    "user": "recipient id",
    "channel": "channel id",
    "ts": "1432132987.001278",
    "text": "This is the body of the message"
}
```

This will send the message to all resources to another user.

```
{
    "type": "message",
    "subtype": "sent",
    "user": "recipient id",
    "channel": "channel id",
    "ts": "1432132987.001278",
    "text": "This is the body of the message"
}
```

#### Read receipt

Client needs to send that it received the messages.

```
{
    "type": "message",
    "subtype": "ack",
    "channel": "channel id",
    "ts": "timestamp of last message"
}
```

It means that it gets all the messages by the last message.

