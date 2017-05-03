-module(iris_req).
-behaviour(gen_server).

-export([start_link/0,
         register_handler/2,
         deregister_handler/1,
         handle/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(handler, {
          type,
          module
         }).

%%%
%%% API functions
%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_handler(SubType, Module) ->
    gen_server:call(?MODULE, {reg_handler, SubType, Module}).

deregister_handler(SubType) ->
    gen_server:call(?MODULE, {dereg_handler, SubType}).

handle(#{<<"type">> := <<"request">>, <<"subtype">> := SubType} = Event) ->
    case ets:lookup(request_handler, SubType) of
        [#handler{module = Module}] ->
            try Module:handle(Event) of
                {ok, _} = Ok ->
                    Ok;
                {error, _} = Error ->
                    Error
            catch
                E:R ->
                    {error, {E, R}}
            end;
        [] ->
            {error, no_handler_for_subtype}
    end.

%%%
%%% gen_server callbacks
%%%

init(_) ->
    ets:new(request_handler, [public, named_table, {keypos, #handler.type}]),
    {ok, undefined}.

handle_call({reg_handler, Type, Module}, _From, State) ->
    %% TODO: error handling
    ets:insert(request_handler, #handler{type = Type, module = Module}),
    {reply, ok, State};
handle_call({dereg_handler, Type}, _From, State) ->
    ets:delete(request_handler, Type),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

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
