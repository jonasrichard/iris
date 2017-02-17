-module(iris_tc_msg).

-export([hello/0,
         msg_create_channel/3,
         msg_message/3,
         msg_read/2]).

hello() ->
    #{type => <<"hello">>}.

msg_create_channel(User, Name, Members) ->
    #{type => <<"channel.create">>,
      user => list_to_binary(User),
      name => list_to_binary(Name),
      invitees => [list_to_binary(M) || M <- Members]}.

msg_message(User, Channel, Text) ->
    #{type => <<"message">>,
      subtype => <<"send">>,
      channel => Channel,
      user => list_to_binary(User),
      text => list_to_binary(Text)}.

msg_read(User, Msg) ->
    #{user => list_to_binary(User),
      type => <<"message">>,
      subtype => <<"read">>,
      ts => maps:get(ts, Msg),
      to => maps:get(user, Msg),
      channel => maps:get(channel, Msg)}.
