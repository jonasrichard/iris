-module(iris_metrics).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    erlang:send_after(10 * 1000, self(), update_metrics),
    {ok, undefined}.

handle_info(update_metrics, State) ->
    do_update_metrics(),
    erlang:send_after(10 * 1000, self(), update_metrics),
    {noreply, State};
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

do_update_metrics() ->
    exometer:update("session.count", count_sessions()).

count_sessions() ->
    Children = supervisor:count_children(iris_client_sup),
    proplists:get_value(active, Children).
