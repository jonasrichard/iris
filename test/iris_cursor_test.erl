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

              Conn2 = iris_tc:login("user2", "pass"),
              Conn3 = iris_tc:login("user3", "pass"),

              Msg1 = iris_tc_msg:msg_message("user1", ChannelId, "Hey all"),
              iris_tc:send(Conn1, Msg1),

              {ok, _Stored1} = iris_tc:wait_for_json(Conn1),
              {ok, _Msg12} = iris_tc:wait_for_json(Conn2),
              {ok, _Msg13} = iris_tc:wait_for_json(Conn3),

              iris_tc:close(Conn3),



              [?_assertEqual(<<"cursor">>, maps:get(name, Channel))]
      end
     }}.

