-module(iris_cursor_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(gun),
    ok.

teardown(_) ->
    ok.

offline_online_test_() ->
    {"Users are mixed online, offline",
     {setup, fun setup/0, fun teardown/1,
      fun(_) ->
              %% user2 will be online
              %% user3 after one message read will be offline
              %% user4 will be offline always
              Conn1 = iris_tc:login("user1", "pass"),

              Create = iris_tc_msg:msg_create_channel("user1", "cursor",
                                                      ["user2", "user3", "user4"]),
              Channel = iris_tc:send_and_wait(Conn1, Create),
              #{id := ChannelId} = Channel,

              _Conn2 = iris_tc:login("user2", "pass"),
              Conn3 = iris_tc:login("user3", "pass"),

              Msg1 = iris_tc_msg:msg_message("user1", ChannelId, "Hey all"),
              iris_tc:send(Conn1, Msg1),

              iris_tc:close(Conn3),
              timer:sleep(100),

              Msg2 = iris_tc_msg:msg_message("user1", ChannelId, "Hey all again"),
              iris_tc:send(Conn1, Msg2),

              R1 = iris_tc:get_read(Conn1, "user1"),
              R2 = iris_tc:get_read(Conn1, "user2"),
              R3 = iris_tc:get_read(Conn1, "user3"),
              R4 = iris_tc:get_read(Conn1, "user4"),

              ?debugVal(R1),
              ?debugVal(R2),
              ?debugVal(R3),
              ?debugVal(R4),

              [?_assertEqual(<<"cursor">>, maps:get(name, Channel))]
      end
     }}.

