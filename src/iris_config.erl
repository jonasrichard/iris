-module(iris_config).
-behaviour(gen_server).

-export([start_link/0,
         get_value/2,
         get_value/3]).

-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

%% TODO in the future, let the user reload the configs
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_value(Section, Default) ->
    case application:get_env(iris, Section) of
        undefined ->
            Default;
        {ok, Val} ->
            Val
    end.

get_value(Section, Property, Default) ->
    case application:get_env(iris, Section) of
        undefined ->
            Default;
        {ok, Val} ->
            case lists:keyfind(Property, 1, Val) of
                false ->
                    Default;
                {_, Val2} ->
                    Val2
            end
    end.

init(_) ->
    {ok, #{}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
