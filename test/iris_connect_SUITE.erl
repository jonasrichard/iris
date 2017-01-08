-module(iris_connect_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [connect].

connect(_) ->
    application:ensure_all_started(gun),
    {ok, Conn} = iris_tc:start_link(),
    iris_tc:send(Conn, #{type => "message"}),
    {ok, Frame} = iris_tc:wait_for_frame(Conn),
    ct:pal("Frame: ~p", [Frame]).
