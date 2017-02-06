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
              _ = iris_tc:wait_for_json(Conn),
              _ = send_and_wait(Conn, msg_auth("user1")),

              Create = msg_create_channel("user1", "friends", ["user2"]),
              Channel = send_and_wait(Conn, Create),

              [?_assertEqual(<<"friends">>, maps:get(<<"name">>, Channel))]
      end
     }}.

send_message_test_() ->
    {"Send message to a channel",
     {setup, fun setup/0, fun teardown/1,
      fun(Conn) ->
              _ = iris_tc:wait_for_json(Conn),
              _ = send_and_wait(Conn, msg_auth("user1")),

              Create = msg_create_channel("user1", "friends2", ["user2"]),
              Channel = send_and_wait(Conn, Create),
              ChannelId = maps:get(<<"id">>, Channel),

              ?debugFmt("Channel created ~p", [ChannelId]),

              Msg = msg_message("user1", ChannelId, "Hey man"),
              iris_tc:send(Conn, Msg),

              HistReq = #{<<"type">> => <<"channel.history">>,
                          <<"channel">> => ChannelId},
              History = send_and_wait(Conn, HistReq),
              ?debugVal(History),
              
              Last = lists:last(maps:get(<<"messages">>, History)),

              [?_assertMatch(#{<<"text">> := <<"Hey man">>}, Last),
               ?_assertMatch(#{<<"user">> := <<"user1">>}, Last)]
      end
     }}.

send_message_other_get_test_() ->
    {"Sending message and other user gets it",
     {setup, fun setup/0, fun teardown/1,
      fun(Conn1) ->
              %% user1 logs in
              _ = iris_tc:wait_for_json(Conn1),
              _ = send_and_wait(Conn1, msg_auth("user1")),

              %% user2 logs in
              {ok, Conn2} = iris_tc:start_link(),
              _ = iris_tc:wait_for_json(Conn2),
              _ = send_and_wait(Conn2, msg_auth("user2")),
              
              %% user1 creates channel and invites user2
              Create = msg_create_channel("user1", "friends3", ["user2"]),
              #{<<"id">> := ChannelId} = Channel = send_and_wait(Conn1, Create),

              %% user2 needs to get the channel info
              {ok, Channel2} = iris_tc:wait_for_json(Conn2),

              %% user1 sends a message
              Msg1 = msg_message("user1", ChannelId, "For sale"),
              iris_tc:send(Conn1, Msg1),

              %% user2 gets the message
              {ok, Msg2} = iris_tc:wait_for_json(Conn2),

              ?debugVal(Msg2),

              [?_assertEqual(Channel, Channel2),
               ?_assertMatch(#{<<"channel">> := ChannelId,
                               <<"user">> := <<"user1">>,
                               <<"text">> := <<"For sale">>,
                               <<"type">> := <<"message">>}, Msg2)]
      end
     }}.

send_and_wait(Conn, Msg) ->
    iris_tc:send(Conn, Msg),
    {ok, Reply} = iris_tc:wait_for_json(Conn),
    Reply.

msg_auth(User) ->
    #{<<"type">> => <<"auth">>,
      <<"user">> => list_to_binary(User),
      <<"pass">> => <<"pass">>}.

msg_create_channel(User, Name, Members) ->
    #{<<"type">> => <<"channel.create">>,
      <<"user">> => list_to_binary(User),
      <<"name">> => list_to_binary(Name),
      <<"invitees">> => [list_to_binary(M) || M <- Members]}.

msg_message(User, Channel, Text) ->
    #{<<"type">> => <<"message">>,
      <<"channel">> => Channel,
      <<"user">> => list_to_binary(User),
      <<"text">> => list_to_binary(Text)}.
