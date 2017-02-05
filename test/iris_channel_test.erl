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
