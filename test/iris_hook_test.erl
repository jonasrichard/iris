-module(iris_hook_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

add_hook_test_() ->
    {"Add more hooks with different priority",
     {setup,
      fun() -> iris_hook:start_link() end,
      fun(_) ->
              iris_hook:add(first, ?MODULE, fun1, 10),
              iris_hook:add(first, ?MODULE, fun2, 20),
              Result = iris_hook:run(first, [[c]]),
              ?_assertEqual([[b, a, c]], Result)
      end}}.

%% TODO: I don't like this. Hook callbacks should have one
%% parameter only.
fun1(List) ->
    {ok, [[a | List]]}.

fun2(List) ->
    {ok, [[b | List]]}.

