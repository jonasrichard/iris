-module(iris_hook).
-behaviour(gen_server).

-export([start_link/0,
         add/4,
         delete/3,
         run/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(hook, {
          name,
          callbacks
         }).

-record(callback, {
          module,
          function,
          priority
         }).

-record(state, {
          hooks
         }).

%%%
%%% API functions
%%%

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    create_default_hooks(),
    {ok, Pid}.

-spec add(atom(), atom(), atom(), integer()) -> any().
add(Hook, Module, Function, Priority) ->
    gen_server:call(?MODULE, {add, Hook, Module, Function, Priority}).

-spec delete(atom(), atom(), atom()) -> any().
delete(Hook, Module, Function) ->
    gen_server:call(?MODULE, {delete, Hook, Module, Function}).

-spec run(atom(), any()) -> ok | {ok, Result::term()} | {error, Reason::term()}.
run(Hook, Args) ->
    case ets:lookup(hooks, Hook) of
        [] ->
            ok;
        [#hook{callbacks = Cbs}] ->
            run_callbacks(Cbs, Args)
    end.

%%%
%%% gen_server callbacks
%%%

init(_) ->
    Tab = ets:new(hooks, [public, named_table, {keypos, #hook.name}]),
    {ok, #state{hooks = Tab}}.

handle_call({add, Hook, Module, Function, Priority}, _From, State) ->
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
    end,
    {reply, ok, State};
handle_call({delete, Hook, Module, Function}, _From, State) ->
    case ets:lookup(hooks, Hook) of
        [] ->
            ok;
        [#hook{callbacks = Cb} = H] ->
            NewCb = lists:filter(
                      fun(#callback{module = M, function = F}) ->
                              M =/= Module orelse F =/= Function
                      end, Cb),
            ets:insert(hooks, H#hook{callbacks = NewCb})
    end,
    {reply, ok, State};
handle_call(_Other, _From, State) ->
    {reply, {error, unknown_message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Internal functions
%%%

create_default_hooks() ->
    ok.
    %%iris_hook:add(message_received, iris_channel, on_message_received, 10).

sort_by_priority(Callbacks) ->
    lists:sort(fun(C1, C2) ->
                       C1#callback.priority =< C2#callback.priority
               end, Callbacks).

run_callbacks([], Args) ->
    Args;
run_callbacks([#callback{module = M, function = F} | Rest], Args) ->
    try erlang:apply(M, F, Args) of
        ok ->
            run_callbacks(Rest, Args);
        {ok, NewArgs} ->
            run_callbacks(Rest, NewArgs);
        {stop, Result} ->
            {ok, Result};
        stop ->
            ok;
        {error, _Reason} = Error ->
            Error
    catch
        Exception:Desc ->
            {error, {Exception, Desc}}
    end.
