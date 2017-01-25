-module(idbg).

-compile(export_all).

client() ->
    redbug:start("iris_client", [{msgs, 100}, {time, 30000}]).

rb(String) ->
    redbug:start(String, [{msgs, 100}, {time, 30000}]).

