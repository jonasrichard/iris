-module(iris_hook_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

add_hook_test_() ->
    {"Add more hooks with different priority",
     {setup,
      fun() -> iris_hook:init('_') end,
      fun(_) ->
              iris_hook:add(first, ?MODULE, fun1, 10),
              iris_hook:add(first, ?MODULE, fun2, 20),
              Result = iris_hook:run(first, [[c]]),
              ?_assertEqual([[b, a, c]], Result)
      end}}.

fun1(List) ->
    {ok, [[a | List]]}.

fun2(List) ->
    {ok, [[b | List]]}.

