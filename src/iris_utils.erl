-module(iris_utils).

-export([id/0,
         ts/0]).

id() ->
    integer_to_binary(erlang:phash2(os:timestamp())).

ts() ->
    {Mega, Sec, Micro} = os:timestamp(),
    Bin = io_lib:format("~p.~6..0B", [Mega * 1000000 + Sec, Micro]),
    iolist_to_binary(Bin).
