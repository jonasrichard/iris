-module(iris_utils).

-export([ts/0]).

ts() ->
    {Mega, Sec, Micro} = os:timestamp(),
    io_lib:format("~p.~6..0B", [Mega * 1000000 + Sec, Micro]).
