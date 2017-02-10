-module(iris_channel_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, Conn} = iris_tc:start_link(),
    Conn.

teardown(Conn) ->
    iris_tc:close(Conn).

create_channel_test_() ->
    {"Create channel",
     {setup, fun setup/0, fun teardown/1,
      fun(Conn) ->
              hello(Conn),
              {ok, _} = iris_tc:authenticate(Conn, "user1", "pass"),

              Create = msg_create_channel("user1", "friends", ["user2"]),
              Channel = send_and_wait(Conn, Create),

              [?_assertEqual(<<"friends">>, maps:get(name, Channel))]
      end
     }}.

send_message_test_() ->
    {"Send message to a channel",
     {setup, fun setup/0, fun teardown/1,
      fun(Conn) ->
              hello(Conn),
              {ok, _} = iris_tc:authenticate(Conn, "user1", "pass"),

              Create = msg_create_channel("user1", "friends2", ["user2"]),
              Channel = send_and_wait(Conn, Create),
              ChannelId = maps:get(id, Channel),

              Msg = msg_message("user1", ChannelId, "Hey man"),
              iris_tc:send(Conn, Msg),

              HistReq = #{type => <<"channel.history">>,
                          channel => ChannelId},
              History = send_and_wait(Conn, HistReq),

              Last = lists:last(maps:get(messages, History)),

              [?_assertMatch(#{text := <<"Hey man">>,
                               user := <<"user1">>}, Last)]
      end
     }}.

send_message_other_get_test_() ->
    {"Sending message and other user gets it",
     {setup, fun setup/0, fun teardown/1,
      fun(Conn1) ->
              %% user1 logs in
              hello(Conn1),
              {ok, _} = iris_tc:authenticate(Conn1, "user1", "pass"),

              %% user2 connects and logs in
              {ok, Conn2} = iris_tc:start_link(),
              hello(Conn2),
              {ok, _} = iris_tc:authenticate(Conn2, "user2", "pass"),

              %% user1 creates channel and invites user2
              Create = msg_create_channel("user1", "friends3", ["user2"]),
              #{id := ChannelId} = Channel = send_and_wait(Conn1, Create),

              %% user2 needs to get the channel info
              {ok, Channel2} = iris_tc:wait_for_json(Conn2),

              %% user1 sends a message
              Msg1 = msg_message("user1", ChannelId, "For sale"),
              iris_tc:send(Conn1, Msg1),

              {ok, Stored} = iris_tc:wait_for_json(Conn1),
              ?debugFmt("Got stored ~p", [Stored]),

              %% user2 gets the message
              {ok, Msg2} = iris_tc:wait_for_json(Conn2),

              %% user2 sends the message receipt
              iris_tc:send(Conn2, msg_read("user2", Msg2)),

              %% user1 gets the message ack
              {ok, Rcpt} = iris_tc:wait_for_json(Conn1),

              #{ts := Msg2Ts} = Msg2,
              #{ts := StoredTs} = Stored,

              [?_assertEqual(Channel, Channel2),
               ?_assertMatch(#{channel := ChannelId,
                               user := <<"user1">>,
                               text := <<"For sale">>,
                               type := <<"message">>}, Msg2),
               ?_assertMatch(#{type := <<"message">>,
                               subtype := <<"stored">>,
                               channel := ChannelId,
                               ts := StoredTs}, Stored),
               ?_assertMatch(#{channel := ChannelId,
                               user := <<"user1">>,
                               from := <<"user2">>,
                               type := <<"message">>,
                               subtype := <<"read">>,
                               ts := Msg2Ts}, Rcpt)]
      end
     }}.

wait(Conn) ->
    {ok, Reply} = iris_tc:wait_for_json(Conn),
    Reply.

send_and_wait(Conn, Msg) ->
    iris_tc:send(Conn, Msg),
    {ok, Reply} = iris_tc:wait_for_json(Conn),
    Reply.

hello(Conn) ->
    #{type := <<"hello">>} = wait(Conn).

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
