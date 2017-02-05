-module(iris_auth_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, Conn} = iris_tc:start_link(),
    Conn.

teardown(Conn) ->
    iris_tc:close(Conn).

auth_test_() ->
    {"Successful authentication",
     {setup, fun setup/0, fun teardown/1,
      fun(Conn) ->
              {ok, Hello} = iris_tc:wait_for_json(Conn),
              iris_tc:send(Conn, #{<<"type">> => <<"auth">>,
                                   <<"user">> => <<"user1">>,
                                   <<"pass">> => <<"pass1">>}),
              {ok, Session} = iris_tc:wait_for_json(Conn),
              [?_assertMatch(#{<<"type">> := <<"hello">>}, Hello),
               ?_assertMatch(#{<<"sessionId">> := _}, Session)]
      end
     }}.

auth_fail_test_() ->
    {"Unsuccessful authentication",
     {setup, fun setup/0, fun teardown/1,
      fun(Conn) ->
              {ok, Hello} = iris_tc:wait_for_json(Conn),
              iris_tc:send(Conn, #{<<"type">> => <<"auth">>,
                                   <<"user">> => <<"no">>,
                                   <<"pass">> => <<"pass1">>}),
              {ok, Error} = iris_tc:wait_for_json(Conn),
              [?_assertMatch(#{<<"type">> := <<"hello">>}, Hello),
               ?_assertMatch(#{<<"type">> := <<"error">>}, Error)]
      end
     }}.
