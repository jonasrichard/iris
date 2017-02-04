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
              ?_assertMatch(#{<<"type">> := <<"hello">>}, Hello)
      end
     }}.
