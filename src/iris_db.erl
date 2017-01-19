-module(iris_db).
-behaviour(gen_server).

-export([start_link/0,
         create_table/3,
         create_table/5]).

-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

start_link() ->
    Backend = iris_config:get_value(database, backend, iris_db_mnesia),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [#{backend => Backend}], []).

create_table(Name, Record, Replicated) ->
    create_table(Name, Record, Replicated, undefined, undefined).

create_table(Name, Record, Replicated, Key, Index) ->
    gen_server:call(?MODULE, {create_table, Name, Record, Replicated, Key, Index}).

init([#{backend := Backend} = State]) ->
    Nodes = [node() | nodes()],
    ok = Backend:init(Nodes),
    {ok, State#{nodes => Nodes, node => node()}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({create_table, Name, Record, Replicated, Persisted, Key, Index}, _From, State) ->
    #{backend := Backend} = State,
    %% TODO check return value
    Backend:create_table(Name, Record, Replicated, Persisted, Key, Index, State),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

