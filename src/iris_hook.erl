-module(iris_hook).

-export([init/1,
         add/4,
         delete/3,
         run/2]).

-record(hook, {
          name,
          callbacks
         }).

-record(callback, {
          module,
          function,
          priority
         }).

init(Opts) ->
    ets:new(hooks, [public, named_table, {keypos, #hook.name}]).

add(Hook, Module, Function, Priority) ->
    NewCb = #callback{module = Module,
                      function = Function,
                      priority = Priority},
    case ets:lookup(hooks, Hook) of
        [] ->
            ets:insert(hooks, #hook{name = Hook,
                                   callbacks = [NewCb]});
        [#hook{callbacks = Cb} = H] ->
            NewH = H#hook{callbacks = sort_by_priority([NewCb | Cb])},
            ets:insert(hooks, NewH)
    end.

delete(Hook, Module, Function) ->
    case ets:lookup(hooks, Hook) of
        [] ->
            ok;
        [#hook{callbacks = Cb} = H] ->
            NewCb = lists:filter(
                      fun(#callback{module = M, function = F}) ->
                              M =/= Module orelse F =/= Function
                      end, Cb),
            ets:insert(hooks, H#hook{callbacks = NewCb})
    end.

run(Hook, Args) ->
    case ets:lookup(hooks, Hook) of
        [] ->
            Args;
        [#hook{callbacks = Cbs}] ->
            run_callbacks(Cbs, Args)
    end.

sort_by_priority(Callbacks) ->
    lists:sort(fun(C1, C2) ->
                       C1#callback.priority =< C2#callback.priority
               end, Callbacks).

run_callbacks([], Args) ->
    Args;
run_callbacks([#callback{module = M, function = F} | Rest], Args) ->
    case erlang:apply(M, F, Args) of
        ok ->
            run_callbacks(Rest, Args);
        {ok, NewArgs} ->
            run_callbacks(Rest, NewArgs);
        stop ->
            ok
    end.

